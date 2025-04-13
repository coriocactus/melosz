{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redis where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.Set as Set
-- import qualified Data.Text.Encoding as TextEnc
import qualified Data.Maybe as Maybe
-- import qualified Data.Foldable as Foldable
import qualified Database.Redis as Redis

import Types
import AppState

type RedisPool = Pool.Pool Redis.Connection

mkRedisConn :: IO Redis.Connection
mkRedisConn = Redis.checkedConnect Redis.defaultConnectInfo
  { Redis.connectHost = "localhost"
  , Redis.connectPort = Redis.PortNumber 6379
  }

mkRedisPool :: IO RedisPool
mkRedisPool = Pool.newPool $ Pool.setNumStripes (Just 100) $
  Pool.defaultPoolConfig mkRedisConn Redis.disconnect 60 100

runRedisAction :: RedisPool -> Redis.Redis a -> IO a
runRedisAction pool action = MonadIO.liftIO $
  Pool.withResource pool $ \conn -> Redis.runRedis conn action

usersIndexKey :: BS.ByteString
usersIndexKey = "users"

userGlickoKey :: UserId -> BS.ByteString
userGlickoKey (UserId uid) = "glicko:" <> uid

optionField :: OptionId -> BS.ByteString
optionField (OptionId oid) = oid

encodeGlicko :: Glicko -> BS.ByteString
encodeGlicko = BSL.toStrict . Aeson.encode

decodeGlicko :: BS.ByteString -> Maybe Glicko
decodeGlicko = Aeson.decode . BSL.fromStrict

decodeGlickoDef :: BS.ByteString -> Glicko
decodeGlickoDef bs = Maybe.fromMaybe initialGlicko (decodeGlicko bs)

-- Redis Schema:
--   `users` (Set): Contains all known UserIds (ByteString).
--   `glicko:<UserId>` (Hash): Maps OptionId (ByteString) -> JSON encoded Glicko (ByteString).

mkRedisHandle :: RedisPool -> Set.Set Option -> StateHandle IO
mkRedisHandle pool options = StateHandle
  { hGetGlicko      = \uid oid -> getGlickoRedis pool uid oid
  , hUpdateRatings  = \uid oid1 g1 oid2 g2 -> updateRatingsRedis pool uid oid1 g1 oid2 g2
  , hGetAllRatings  = \uid -> getAllRatingsRedis pool options uid
  , hEnsureUser     = \uid opts -> ensureUserRedis pool uid opts
  }

-- | Retrieves the Glicko rating for a specific user and option.
-- If the user or option doesn't exist in Redis, returns the initial Glicko rating.
getGlickoRedis :: RedisPool -> UserId -> OptionId -> IO Glicko
getGlickoRedis pool uid oid = do
  let uKey = userGlickoKey uid
      oField = optionField oid
  result <- runRedisAction pool $ Redis.hget uKey oField
  case result of
    Left _ -> pure initialGlicko
    Right Nothing -> pure initialGlicko
    Right (Just glickoBS) -> pure $ decodeGlickoDef glickoBS

-- | Atomically updates the Glicko ratings for two options for a given user.
-- This function assumes the user might already exist. If the user is guaranteed
-- to exist via ensureUserRedis, the SADD could be omitted, but it's safer here.
updateRatingsRedis :: RedisPool -> UserId -> OptionId -> Glicko -> OptionId -> Glicko -> IO ()
updateRatingsRedis pool uid oid1 g1 oid2 g2 = do
  let uKey = userGlickoKey uid
      (UserId userIdBS) = uid
      oField1 = optionField oid1
      oField2 = optionField oid2
      g1Json = encodeGlicko g1
      g2Json = encodeGlicko g2

  _ <- runRedisAction pool $ Redis.multiExec $ do
    _ <- Redis.sadd usersIndexKey [userIdBS]
    _ <- Redis.hset uKey oField1 g1Json
    _ <- Redis.hset uKey oField2 g2Json
    return $ pure ()
  return ()

-- | Retrieves all Glicko ratings for a given user for the set of globally known options.
-- If the user doesn't exist in Redis, returns a map with initial Glicko ratings for all options.
-- If the user exists, it retrieves their ratings, defaulting to initial values for any missing options.
getAllRatingsRedis :: RedisPool -> Set.Set Option -> UserId -> IO (Map.Map OptionId Glicko)
getAllRatingsRedis pool options uid = do
  let uKey = userGlickoKey uid
      allOptionIds = Set.map optionId options

  result <- runRedisAction pool $ Redis.hgetall uKey
  case result of
    Left _ -> pure $ Map.fromSet (const initialGlicko) allOptionIds
    Right fieldsAndValues -> do
      let redisMap = parseHGetAllResponse fieldsAndValues
          finalMap = Map.fromSet (lookupOrDefault redisMap) allOptionIds
      pure finalMap
  where
    parseHGetAllResponse :: [(BS.ByteString, BS.ByteString)] -> Map.Map OptionId Glicko
    parseHGetAllResponse pairs = Map.fromList $ Maybe.mapMaybe parsePair pairs

    parsePair :: (BS.ByteString, BS.ByteString) -> Maybe (OptionId, Glicko)
    parsePair (oidBS, glickoBS) =
      case decodeGlicko glickoBS of
        Just glicko -> Just (OptionId oidBS, glicko)
        Nothing -> Nothing

    lookupOrDefault :: Map.Map OptionId Glicko -> OptionId -> Glicko
    lookupOrDefault redisMap oid = Map.findWithDefault initialGlicko oid redisMap

-- | Ensures that a user exists in Redis and has at least initial Glicko ratings
-- for all specified options. Uses HSETNX to avoid overwriting existing ratings.
ensureUserRedis :: RedisPool -> UserId -> Set.Set Option -> IO ()
ensureUserRedis pool uid opts = do
  let uKey = userGlickoKey uid
      (UserId userIdBS) = uid
      initialGlickoJson = encodeGlicko initialGlicko
      optionFields = Set.toList $ Set.map optionField (Set.map optionId opts)

  _ <- runRedisAction pool $ Redis.multiExec $ do
    _ <- Redis.sadd usersIndexKey [userIdBS]
    Monad.mapM_ (\oField -> Redis.hsetnx uKey oField initialGlickoJson) optionFields
    return $ pure ()
  return ()

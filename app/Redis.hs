{-# LANGUAGE OverloadedStrings #-}

module Redis where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time.Clock.POSIX as POSIXTime
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

mkRedisHandle :: RedisPool -> Set.Set Option -> StateHandle IO
mkRedisHandle pool options = StateHandle
  { hGetGlicko      = \uid oid -> getGlickoRedis pool uid oid
  , hUpdateRatings  = \uid oid1 g1 oid2 g2 -> updateRatingsRedis pool uid oid1 g1 oid2 g2
  , hGetAllRatings  = \uid -> getAllRatingsRedis pool options uid
  , hEnsureUser     = \uid opts -> ensureUserRedis pool uid opts
  }

----

-- | Redis Schema (note: email = userId)
-- [hash] "tokens": [<token> -> <email>]
-- [hash] "hashes": [<hash> -> <email>]
-- [set]  "users" -> [<email>]
-- [hash] [user:<email>] -> [hash -> <hash>, created_on -> <created_on>, accessed_on -> <accessed_on>]
-- [hash] [glicko:<email>] -> [<oid> -> <glicko_json>]

userMetaKey :: BS.ByteString -> BS.ByteString
userMetaKey email = "user:" <> email

encodeString :: String -> BS.ByteString
encodeString str = BSU.fromString str

encodeText :: Text.Text -> BS.ByteString
encodeText text = TextEnc.encodeUtf8 text

encodeTimestamp :: POSIXTime.POSIXTime -> BS.ByteString
encodeTimestamp now = BSU.fromString $ show (floor now :: Int)

hExpire :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Redis.Redis (Either Redis.Reply Redis.Status)
hExpire key seconds field = Redis.sendRequest ["HEXPIRE", key, seconds, "FIELDS", "1", field]

setTokenRedis :: BS.ByteString -> BS.ByteString -> Redis.Redis (Redis.TxResult ())
setTokenRedis token email = Redis.multiExec $ do
  _ <- Redis.hset "tokens" token email
  _ <- Redis.liftRedis $ hExpire "tokens" "300" token
  return $ pure ()

getTokenRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f (Maybe BSU.ByteString))
getTokenRedis token = Redis.hget "tokens" token

delTokenRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f Integer)
delTokenRedis token = Redis.hdel "tokens" [token]

findUserByEmailRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f Bool)
findUserByEmailRedis email = Redis.sismember "users" email

getUserByHashRedis :: Text.Text -> Redis.Redis (Either Redis.Reply (Maybe (Bool, UserId)))
getUserByHashRedis hash = do
  isValidHash <- Redis.hget "hashes" (encodeText hash)
  case isValidHash of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Right Nothing
    Right (Just email) -> do
      isRegistered <- findUserByEmailRedis email
      case isRegistered of
        Left err -> pure $ Left err
        Right False -> pure $ Right (Just $ (False, UserId email))
        Right True -> pure $ Right (Just $ (True, UserId email))

loginRedis :: BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
loginRedis now email hash = Redis.multiExec $ do
  _ <- Redis.hset "hashes" hash email
  _ <- Redis.hset (userMetaKey email) "hash" hash
  _ <- Redis.hset (userMetaKey email) "accessed_on" now
  return $ pure ()

registerRedis :: BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
registerRedis now email hash = Redis.multiExec $ do
  _ <- Redis.sadd "users" [email]
  _ <- Redis.hset "hashes" hash email
  _ <- Redis.hset (userMetaKey email) "hash" hash
  _ <- Redis.hset (userMetaKey email) "created_on" now
  _ <- Redis.hset (userMetaKey email) "accessed_on" now
  return $ pure ()

makeGuestRedis :: BSU.ByteString -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
makeGuestRedis email hash = Redis.multiExec $ do
  _ <- Redis.hset "hashes" hash email
  _ <- Redis.liftRedis $ hExpire "hashes" "3600" hash
  return $ pure ()

extendGuestRedis :: UserId -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
extendGuestRedis uid hash = Redis.multiExec $ do
  _ <- Redis.liftRedis $ hExpire "hashes" "3600" hash
  _ <- Redis.expire (userGlickoKey uid) 3600
  return $ pure ()

findHashesForEmail :: BS.ByteString -> Redis.Redis (Either Redis.Reply [BS.ByteString])
findHashesForEmail email = do
  result <- Redis.hgetall "hashes"
  case result of
    Left err -> pure $ Left err
    Right allPairs -> do
      let userHashes = [h | (h, e) <- allPairs, e == email]
      pure $ Right userHashes

logoutRedis :: BS.ByteString -> Redis.Redis (Either Redis.Reply Integer)
logoutRedis email = do
  findResult <- findHashesForEmail email
  case findResult of
    Left err -> pure $ Left err
    Right [] -> pure $ Right 0
    Right userHashes -> Redis.hdel "hashes" userHashes

deleteRedis :: BS.ByteString -> Redis.Redis (Either Redis.Reply (Redis.TxResult ()))
deleteRedis email = do
  findResult <- findHashesForEmail email
  case findResult of
    Left err -> pure $ Left err
    Right userHashes -> do
      let userMeta = userMetaKey email
          userGlicko = userGlickoKey (UserId email)

      txResult <- Redis.multiExec $ do
        Monad.unless (null userHashes) $
          Redis.hdel "hashes" userHashes >> return ()
        _ <- Redis.srem "users" [email]
        _ <- Redis.del [userMeta, userGlicko]
        pure $ pure ()

      pure $ Right txResult

----

recentComparisonHistorySize :: Int
recentComparisonHistorySize = 10

recentCompsListKey :: UserId -> BS.ByteString
recentCompsListKey (UserId uid) = "glicko:" <> uid <> ":recents"

pairToRedisKey :: (Option, Option) -> BS.ByteString
pairToRedisKey (o1, o2) =
  let (OptionId id1) = optionId o1
      (OptionId id2) = optionId o2
  in if id1 <= id2 then id1 <> ":" <> id2 else id2 <> ":" <> id1

recordRecentComparisonListRedis :: RedisPool -> UserId -> (Option, Option) -> IO (Either Redis.Reply ())
recordRecentComparisonListRedis pool uid pair = do
  let key = recentCompsListKey uid
      member = pairToRedisKey pair
      maxSize = recentComparisonHistorySize
      maxIndex = fromIntegral (max 0 (maxSize - 1))
  result <- runRedisAction pool $ Redis.multiExec $ do
    _ <- Redis.lpush key [member]
    _ <- Redis.ltrim key 0 maxIndex
    pure $ pure ()
  case result of
    Redis.TxError err -> pure $ Left (Redis.Error $ BSC.pack $ "Redis transaction error: " ++ err)
    Redis.TxAborted -> pure $ Left (Redis.Error "Redis transaction aborted")
    Redis.TxSuccess _ -> pure $ Right ()

getRecentComparisonListKeysRedis :: RedisPool -> UserId -> IO (Either Redis.Reply (Set.Set BS.ByteString))
getRecentComparisonListKeysRedis pool uid = do
  let key = recentCompsListKey uid
      maxSize = recentComparisonHistorySize
      maxIndex = fromIntegral (max 0 (maxSize - 1))
  result <- runRedisAction pool $ Redis.lrange key 0 maxIndex
  case result of
    Left err -> pure $ Left err
    Right members -> pure $ Right $ Set.fromList members

----

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
updateRatingsRedis :: RedisPool -> UserId -> OptionId -> Glicko -> OptionId -> Glicko -> IO ()
updateRatingsRedis pool uid oid1 g1 oid2 g2 = do
  let uKey = userGlickoKey uid
      oField1 = optionField oid1
      oField2 = optionField oid2
      g1Json = encodeGlicko g1
      g2Json = encodeGlicko g2

  _ <- runRedisAction pool $ Redis.multiExec $ do
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

-- | Ensures that a user has at least initial Glicko ratings for all specified
-- options. Uses HSETNX to avoid overwriting existing ratings.
ensureUserRedis :: RedisPool -> UserId -> Set.Set Option -> IO ()
ensureUserRedis pool uid opts = do
  let uKey = userGlickoKey uid
      initialGlickoJson = encodeGlicko initialGlicko
      optionFields = Set.toList $ Set.map optionField (Set.map optionId opts)

  _ <- runRedisAction pool $ Redis.multiExec $ do
    Monad.mapM_ (\oField -> Redis.hsetnx uKey oField initialGlickoJson) optionFields
    return $ pure ()
  return ()

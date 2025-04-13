{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redis where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.Set as Set
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

encodeGlicko :: Glicko -> BSC.ByteString
encodeGlicko = BSL.toStrict . Aeson.encode

decodeGlicko :: BSC.ByteString -> Maybe Glicko
decodeGlicko = Aeson.decode . BSL.fromStrict

mkRedisHandle :: RedisPool -> Set.Set Option -> StateHandle IO
mkRedisHandle pool options = StateHandle
  { hGetGlicko      = \uid oid -> getGlickoRedis pool uid oid
  , hUpdateRatings  = \uid oid1 g1 oid2 g2 -> updateRatingsRedis pool uid oid1 g1 oid2 g2
  , hGetAllRatings  = \uid -> getAllRatingsRedis pool options uid
  , hEnsureUser     = \uid userOpts -> ensureUserRedis pool uid userOpts
  }

getGlickoRedis :: RedisPool -> UserId -> OptionId -> IO Glicko
getGlickoRedis = undefined

updateRatingsRedis :: RedisPool -> UserId -> OptionId -> Glicko -> OptionId -> Glicko -> IO ()
updateRatingsRedis = undefined

getAllRatingsRedis :: RedisPool -> Set.Set Option -> UserId -> IO (Map.Map OptionId Glicko)
getAllRatingsRedis = undefined

ensureUserRedis :: RedisPool -> UserId -> Set.Set Option -> IO ()
ensureUserRedis = undefined

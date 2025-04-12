{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Redis where

import qualified Data.Pool as Pool
import qualified Database.Redis as Redis

type RedisPool = Pool.Pool Redis.Connection

mkRedisConn :: IO Redis.Connection
mkRedisConn = Redis.checkedConnect Redis.defaultConnectInfo
  { Redis.connectHost = "localhost"
  , Redis.connectPort = Redis.PortNumber 6379
  }

mkRedisPool :: IO RedisPool
mkRedisPool = Pool.newPool $ Pool.setNumStripes (Just 100) $
  Pool.defaultPoolConfig mkRedisConn Redis.disconnect 60 100

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Set as Set
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RL

import Servant

import Types
import AppState

import Redis
import Auth
import Templates
import Home
import Account
import Megusta

-- application

main :: IO ()
main = launch 5002

launch :: Int -> IO ()
launch port = do
  let options = colourfulOptions

  pool <- mkRedisPool
  let redisHandle = mkRedisHandle pool options

  let config = AppConfig
        { configOptions = options
        , configSystemTau = 0.5
        , configStateHandle = redisHandle
        }

  putStrLn $ "=== === === running melosz backend === === ==="
  putStrLn $ "listening: http://localhost:" ++ show port

  Warp.run port (application config pool)

colourfulOptions :: Set.Set Option
colourfulOptions = Set.fromList
  [ createOption "red" "Red" ""
  , createOption "orange" "Orange" ""
  , createOption "yellow" "Yellow" ""
  , createOption "green" "Green" ""
  , createOption "blue" "Blue" ""
  , createOption "violet" "Violet" ""
  , createOption "indigo" "Indigo" ""
  , createOption "cyan" "Cyan" ""
  , createOption "magenta" "Magenta" ""
  ]

-- webserver

application :: AppConfig -> RedisPool -> Wai.Application
application cfg pool = Gzip.gzip Gzip.defaultGzipSettings $ RL.logStdout $
  serveWithContext butler underButler (servants cfg pool)

butler :: Proxy API
butler = Proxy

underButler :: Context '[ErrorFormatters]
underButler = errorFormatters :. EmptyContext

servants :: AppConfig -> RedisPool -> Server API
servants cfg pool = staticServant :<|> authServant pool
  :<|> homeServant cfg pool
  :<|> accountServant cfg pool
  :<|> megustaServant cfg pool

type API = StaticAPI :<|> AuthAPI
  :<|> Protect :> HomeAPI
  :<|> Protect :> AccountAPI
  :<|> Protect :> MegustaAPI

staticServant :: Server StaticAPI
staticServant = serveDirectoryWebApp "styles"

type StylesAPI = "styles" :> Raw
type StaticAPI = StylesAPI

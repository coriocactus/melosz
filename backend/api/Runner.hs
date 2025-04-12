{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Runner where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Pool as Pool
import qualified Database.Redis as Redis

import Servant

import AppState

run :: Either ServerError a -> Handler a
run action = case action of
  Left err -> throwError err
  Right val -> pure val

runStrict :: a -> Handler a
runStrict action = pure action

runApp :: AppConfig -> App (Either ServerError a) -> Handler a
runApp cfg action = do
  result <- MonadIO.liftIO $ MonadReader.runReaderT action cfg
  case result of
    Left err -> throwError err
    Right val -> pure val

runAppStrict :: AppConfig -> App a -> Handler a
runAppStrict cfg action = MonadIO.liftIO $ MonadReader.runReaderT action cfg

execRedis :: Pool.Pool Redis.Connection -> Redis.Redis a -> Handler a
execRedis pool redisAction = MonadIO.liftIO $
  Pool.withResource pool $ \conn -> Redis.runRedis conn redisAction

{-# LANGUAGE OverloadedStrings #-}

module Actions where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Pool as Pool
import qualified Database.Redis as Redis

import Servant

import AppState

exec :: Either ServerError a -> Handler a
exec action = case action of
  Left err -> throwError err
  Right val -> pure val

execApp :: AppConfig -> App (Either ServerError a) -> Handler a
execApp cfg action = do
  result <- MonadIO.liftIO $ MonadReader.runReaderT action cfg
  case result of
    Left err -> throwError err
    Right val -> pure val

execRedis :: Pool.Pool Redis.Connection -> Redis.Redis a -> Handler a
execRedis pool redisAction = MonadIO.liftIO $
  Pool.withResource pool $ \conn -> Redis.runRedis conn redisAction

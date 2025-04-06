{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.IORef as IORef

import Types
import AppState
import Marshal
import Console

runner :: App a -> IO a
runner appToRun = do
  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let config = AppConfig
        { configKFactor = 32.0
        , configInitialRating = 1500.0
        , configStateRef = initialStateRef
        }
  MonadReader.runReaderT appToRun config

colourfulScaffold :: App ([Option], UserId)
colourfulScaffold = do
  let options =
        [ createOption "red" "Red"
        , createOption "orange" "Orange"
        , createOption "yellow" "Yellow"
        , createOption "green" "Green"
        , createOption "blue" "Blue"
        , createOption "violet" "Violet"
        , createOption "indigo" "Indigo"
        , createOption "cyan" "Cyan"
        , createOption "magenta" "Magenta"
        ]
  let user = "coriocactus"

  setupOptions options
  setupUser user

  pure (options, user)

colourful :: App ()
colourful = colourfulScaffold >>=
  (\(options, user) -> runInteractiveSession user options)

main :: IO ()
main = runner colourful

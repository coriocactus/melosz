{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState

import Types
import AppState
import Marshal
import Console

runner :: App a -> IO a
runner appToRun =
  MonadState.evalStateT (MonadReader.runReaderT appToRun defaultConfig) initialState

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

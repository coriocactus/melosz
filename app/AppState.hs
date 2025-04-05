{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppState where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad

import Types

type App = MonadReader.ReaderT AppConfig (MonadState.StateT AppState IO)

data AppConfig = AppConfig
  { configKFactor :: Double
  , configInitialRating :: Double
  }

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configKFactor = 32.0
  , configInitialRating = 1500.0
  }

getConfig :: App AppConfig
getConfig = MonadReader.ask

data AppState = AppState
  { stateUsers :: Set.Set UserId
  , stateOptions :: Set.Set Option
  , stateRatings :: Map.Map (UserId, OptionId) Double
  , statePreferences :: Map.Map UserId Relation
  , stateNextTimestamp :: Int
  , stateViolations :: Map.Map UserId (Set.Set (Option, Option, Option))
  , stateUncomparedPairs :: Map.Map UserId (Set.Set (Option, Option))
  }

getOptions :: App (Set.Set Option)
getOptions = MonadState.gets stateOptions

initialState :: AppState
initialState = AppState
  { stateUsers = Set.empty
  , stateOptions = Set.empty
  , stateRatings = Map.empty
  , statePreferences = Map.empty
  , stateNextTimestamp = 0
  , stateViolations = Map.empty
  , stateUncomparedPairs = Map.empty
  }

getUsers :: App (Set.Set UserId)
getUsers = MonadState.gets stateUsers

getPreferencesForUser :: UserId -> App Relation
getPreferencesForUser userId = MonadState.gets (Map.findWithDefault Set.empty userId . statePreferences)

getUncomparedPairsForUser :: UserId -> App (Set.Set (Option, Option))
getUncomparedPairsForUser userId = MonadState.gets (Map.findWithDefault Set.empty userId . stateUncomparedPairs)

getViolationsForUser :: UserId -> App (Set.Set (Option, Option, Option))
getViolationsForUser userId = MonadState.gets (Map.findWithDefault Set.empty userId . stateViolations)

getRatingsMap :: App (Map.Map (UserId, OptionId) Double)
getRatingsMap = MonadState.gets stateRatings

getNextTimestamp :: App Int
getNextTimestamp = MonadState.gets stateNextTimestamp

getAllOptionPairsSet :: Set.Set Option -> Set.Set (Option, Option)
getAllOptionPairsSet options = Set.fromList $ do
  o1 <- Set.toList options
  o2 <- Set.toList options
  Monad.guard (optionId o1 < optionId o2)
  pure (o1, o2)

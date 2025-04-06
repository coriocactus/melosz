{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppState where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe

import Types

type App = MonadReader.ReaderT AppConfig (MonadState.StateT AppState IO)

-- === Configuration ===

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

-- === User State ===

data UserState = UserState
  { userRatings :: Map.Map OptionId Double
  , userPreferences :: Relation
  , userViolations :: Set.Set (Option, Option, Option)
  , userUncomparedPairs :: Set.Set (Option, Option)
  } deriving (Show, Eq)

initialUserState :: Set.Set Option -> UserState
initialUserState options = UserState
  { userRatings = Map.empty
  , userPreferences = Set.empty
  , userViolations = Set.empty
  , userUncomparedPairs = getAllOptionPairsSet options
  }

-- === Global Application State ===

data AppState = AppState
  { stateOptions :: Set.Set Option
  , stateUserStates :: Map.Map UserId UserState
  , stateNextTimestamp :: Int
  }

initialState :: AppState
initialState = AppState
  { stateOptions = Set.empty
  , stateUserStates = Map.empty
  , stateNextTimestamp = 0
  }

-- === State Accessors ===

getOptions :: App (Set.Set Option)
getOptions = MonadState.gets stateOptions

getUsers :: App (Set.Set UserId)
getUsers = MonadState.gets (Map.keysSet . stateUserStates)

getNextTimestamp :: App Int
getNextTimestamp = MonadState.gets stateNextTimestamp

getUserState :: UserId -> App (Maybe UserState)
getUserState userId = MonadState.gets (Map.lookup userId . stateUserStates)

-- helper to get userstate, providing a default empty state
getUserState' :: UserId -> App UserState
getUserState' userId = Maybe.fromMaybe (initialUserState Set.empty) <$> getUserState userId

getPreferencesForUser :: UserId -> App Relation
getPreferencesForUser userId = userPreferences <$> getUserState' userId

getUncomparedPairsForUser :: UserId -> App (Set.Set (Option, Option))
getUncomparedPairsForUser userId = userUncomparedPairs <$> getUserState' userId

getViolationsForUser :: UserId -> App (Set.Set (Option, Option, Option))
getViolationsForUser userId = userViolations <$> getUserState' userId

-- === Utility ===

getAllOptionPairsSet :: Set.Set Option -> Set.Set (Option, Option)
getAllOptionPairsSet options = Set.fromList $ do
  o1 <- Set.toList options
  o2 <- Set.toList options
  Monad.guard (optionId o1 < optionId o2)
  pure (o1, o2)

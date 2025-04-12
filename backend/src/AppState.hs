{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppState where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.IORef as IORef

import Types

-- === Application Monad ===

type App = MonadReader.ReaderT AppConfig IO

-- === Configuration ===

data AppConfig = AppConfig
  { configSystemTau :: Double
  , configStateRef :: IORef.IORef AppState
  }

getConfig :: App AppConfig
getConfig = MonadReader.ask

-- === Global Application State ===

data AppState = AppState
  { stateOptions :: Set.Set Option
  , stateUserStates :: Map.Map UserId UserState
  } deriving (Show, Eq)

initialState :: AppState
initialState = AppState
  { stateOptions = Set.empty
  , stateUserStates = Map.empty
  }

-- === User State ===

data UserState = UserState
  { userGlickos :: Map.Map OptionId Glicko
  , userPreferences :: Relation
  , userViolations :: Set.Set (Option, Option, Option)
  , userUncomparedPairs :: Set.Set (Option, Option)
  } deriving (Show, Eq)

initialUserState :: Set.Set Option -> UserState
initialUserState options = UserState
  { userGlickos = Map.fromSet (const initialGlicko) (Set.map optionId options)
  , userPreferences = Set.empty
  , userViolations = Set.empty
  , userUncomparedPairs = getAllOptionPairsSet options
  }

-- === State Accessors ===

readCurrentState :: App AppState
readCurrentState = do
  ref <- MonadReader.asks configStateRef
  MonadIO.liftIO $ IORef.readIORef ref

getOptions :: App (Set.Set Option)
getOptions = stateOptions <$> readCurrentState

getUsers :: App (Set.Set UserId)
getUsers = Map.keysSet . stateUserStates <$> readCurrentState

getUserState :: UserId -> App (Maybe UserState)
getUserState userId = Map.lookup userId . stateUserStates <$> readCurrentState

getUserState' :: UserId -> App UserState
getUserState' userId = Maybe.fromMaybe (initialUserState Set.empty) <$> getUserState userId

getGlicko :: UserId -> OptionId -> App (Maybe Glicko)
getGlicko userId oid = do
  mUserState <- getUserState userId
  pure $ mUserState >>= Map.lookup oid . userGlickos

getGlicko' :: UserId -> OptionId -> App Glicko
getGlicko' userId oid = Maybe.fromMaybe initialGlicko <$> getGlicko userId oid

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

modifyStateRef :: (AppState -> (AppState, a)) -> App a
modifyStateRef f = do
  stateRef <- MonadReader.asks configStateRef
  MonadIO.liftIO $ IORef.atomicModifyIORef' stateRef f

modifyStateRef_ :: (AppState -> AppState) -> App ()
modifyStateRef_ f = modifyStateRef (\s -> (f s, ()))

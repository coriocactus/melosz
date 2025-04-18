{-# LANGUAGE OverloadedStrings #-}

module AppState where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Types

type App a = MonadReader.ReaderT AppConfig IO a

data AppConfig = AppConfig
  { configOptions :: Set.Set Option
  , configSystemTau :: Double
  , configStateHandle :: StateHandle IO
  }

class Monad m => MonadAppState m where
  getStorableGlicko :: UserId -> OptionId -> m Glicko
  updateStorableRatings :: UserId -> OptionId -> Glicko -> OptionId -> Glicko -> m ()
  getAllStorableRatings :: UserId -> m (Map.Map OptionId Glicko)
  ensureStorableUser :: UserId -> Set.Set Option -> m ()

instance MonadAppState (MonadReader.ReaderT AppConfig IO) where
  getStorableGlicko uid oid = do
    handle <- MonadReader.asks configStateHandle
    MonadIO.liftIO $ hGetGlicko handle uid oid
  updateStorableRatings uid oid1 g1 oid2 g2 = do
    handle <- MonadReader.asks configStateHandle
    MonadIO.liftIO $ hUpdateRatings handle uid oid1 g1 oid2 g2
  getAllStorableRatings uid = do
    handle <- MonadReader.asks configStateHandle
    MonadIO.liftIO $ hGetAllRatings handle uid
  ensureStorableUser uid opts = do
    handle <- MonadReader.asks configStateHandle
    MonadIO.liftIO $ hEnsureUser handle uid opts

data StateHandle m = StateHandle
  { hGetGlicko      :: UserId -> OptionId -> m Glicko
  , hUpdateRatings  :: UserId -> OptionId -> Glicko -> OptionId -> Glicko -> m ()
  , hGetAllRatings  :: UserId -> m (Map.Map OptionId Glicko)
  , hEnsureUser     :: UserId -> Set.Set Option -> m ()
  }

getConfig :: App AppConfig
getConfig = MonadReader.ask

getAllOptionPairsSet :: Set.Set Option -> Set.Set (Option, Option)
getAllOptionPairsSet options = Set.fromList $ do
  o1 <- Set.toList options
  o2 <- Set.toList options
  Monad.guard (optionId o1 < optionId o2)
  pure (makeCanonicalPair o1 o2)

getOptionById :: OptionId -> App (Maybe Option)
getOptionById oidToFind = do
  optionsSet <- MonadReader.asks configOptions
  pure $ List.find (\opt -> optionId opt == oidToFind) (Set.toList optionsSet)

setupUser :: (MonadAppState m, MonadReader.MonadReader AppConfig m) => UserId -> m ()
setupUser userId = do
  optionsSet <- MonadReader.asks configOptions
  ensureStorableUser userId optionsSet

setupUsers :: (MonadAppState m, MonadReader.MonadReader AppConfig m) => [UserId] -> m ()
setupUsers users = Monad.forM_ users setupUser

-- (base) ioref state implementation

data AppState = AppState
  { stateUserStates :: Map.Map UserId UserState
  } deriving (Show, Eq)

initialAppState :: AppState
initialAppState = AppState
  { stateUserStates = Map.empty
  }

data UserState = UserState
  { userGlickos :: Map.Map OptionId Glicko
  } deriving (Show, Eq)

initialUserState :: Set.Set Option -> UserState
initialUserState options = UserState
  { userGlickos = Map.fromSet (const initialGlicko) (Set.map optionId options)
  }

mkIORefHandle :: IORef.IORef AppState -> StateHandle IO
mkIORefHandle ref = StateHandle
  { hGetGlicko = \uid oid -> getGlickoIORef ref uid oid
  , hUpdateRatings = \uid oid1 g1 oid2 g2 -> updateRatingsIORef ref uid oid1 g1 oid2 g2
  , hGetAllRatings = \uid -> getAllRatingsIORef ref uid
  , hEnsureUser = \uid opts -> ensureUserIORef ref uid opts
  }

getGlickoIORef :: IORef.IORef AppState -> UserId -> OptionId -> IO Glicko
getGlickoIORef ref uid oid = do
  s <- IORef.readIORef ref
  pure $ Maybe.fromMaybe initialGlicko $
    Map.lookup uid (stateUserStates s) >>= Map.lookup oid . userGlickos

updateRatingsIORef :: IORef.IORef AppState -> UserId -> OptionId -> Glicko -> OptionId -> Glicko -> IO ()
updateRatingsIORef ref uid oid1 g1 oid2 g2 = do
  IORef.atomicModifyIORef' ref $ \s ->
    let updateGlickos uMap = Map.insert oid1 g1 $ Map.insert oid2 g2 uMap
        modifyUserState uState = uState { userGlickos = updateGlickos (userGlickos uState) }
        modifyAppMap appMap = Map.adjust modifyUserState uid appMap
        newState = if Map.member uid (stateUserStates s) then s { stateUserStates = modifyAppMap (stateUserStates s) } else s
    in (newState, ())

getAllRatingsIORef :: IORef.IORef AppState -> UserId -> IO (Map.Map OptionId Glicko)
getAllRatingsIORef ref uid = do
  s <- IORef.readIORef ref
  pure $ maybe Map.empty userGlickos (Map.lookup uid (stateUserStates s))

ensureUserIORef :: IORef.IORef AppState -> UserId -> Set.Set Option -> IO ()
ensureUserIORef ref uid opts = do
  IORef.atomicModifyIORef' ref $ \s ->
    if Map.member uid (stateUserStates s) then (s, ())
    else
      let newUserState = initialUserState opts
          newMap = Map.insert uid newUserState (stateUserStates s)
      in (s { stateUserStates = newMap }, ())

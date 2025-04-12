{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Control.Monad.Reader as MonadReader
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef

import Test.Hspec

import Types
import AppState

defaultTestConfig :: AppConfig
defaultTestConfig = AppConfig
  { configSystemTau = 0.5
  , configStateRef = error "configStateRef should be set by test runner"
  }

evalAppTest :: App a -> Maybe AppConfig -> AppState -> IO a
evalAppTest action mConfig initState =
  fst <$> runAppTest action mConfig initState

execAppTest :: App a -> Maybe AppConfig -> AppState -> IO AppState
execAppTest action mConfig initState = do
  ref <- IORef.newIORef initState
  (_, finalRef) <- runAppTestRef action mConfig ref
  IORef.readIORef finalRef

runAppTest :: App a -> Maybe AppConfig -> AppState -> IO (a, AppState)
runAppTest action mConfig initState = do
  ref <- IORef.newIORef initState
  (result, finalRef) <- runAppTestRef action mConfig ref
  finalState <- IORef.readIORef finalRef
  pure (result, finalState)

runAppTestRef :: App a -> Maybe AppConfig -> IORef.IORef AppState -> IO (a, IORef.IORef AppState)
runAppTestRef action mConfig stateRef = do
  let config = Maybe.fromMaybe defaultTestConfig mConfig
      finalConfig = config { configStateRef = stateRef }
  result <- MonadReader.runReaderT action finalConfig
  pure (result, stateRef)

shouldBeApproxPrec :: (Fractional a, Ord a, Show a) => a -> a -> a -> Expectation
shouldBeApproxPrec margin actual expected =
  if abs (actual - expected) < abs margin * max 1 (abs expected)
    then return ()
    else expectationFailure message
  where
    message = concat [
      "Test Failed\nexpected: ", show expected,
      " within margin of ", show margin,
      "\n but got: ", show actual]

shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldBeApprox = shouldBeApproxPrec 1e-6

testUser1 :: UserId
testUser1 = "user1"

testUser2 :: UserId
testUser2 = "user2"

optA, optB, optC, optD, optE :: Option
optA = createOption "A" "Option A" ""
optB = createOption "B" "Option B" ""
optC = createOption "C" "Option C" ""
optD = createOption "D" "Option D" ""
optE = createOption "E" "Option E" ""

allTestOptions :: [Option]
allTestOptions = [optA, optB, optC, optD]

allTestOptionsSet :: Set.Set Option
allTestOptionsSet = Set.fromList allTestOptions

mkAppState :: Set.Set Option -> [(UserId, UserState)] -> AppState
mkAppState options userStatesList = initialState
  { stateOptions = options
  , stateUserStates = Map.fromList userStatesList
  }

mkUserState :: Map.Map OptionId Glicko -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> UserState
mkUserState glickoMap prefs violations uncompared = UserState
  { userGlickos = glickoMap
  , userPreferences = prefs
  , userViolations = violations
  , userUncomparedPairs = uncompared
  }

setupStateSingleUser :: UserId -> Set.Set Option -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> AppState
setupStateSingleUser userId options prefs violations uncompared =
  let glicko = Map.fromSet (const initialGlicko) (Set.map optionId options)
      uState = mkUserState glicko prefs violations uncompared
  in mkAppState options [(userId, uState)]

simpleUserState :: Set.Set Option -> UserState
simpleUserState options =
  let glicko = Map.fromSet (const initialGlicko) (Set.map optionId options)
      initialUncompared = getAllOptionPairsSet options
  in mkUserState glicko Set.empty Set.empty initialUncompared

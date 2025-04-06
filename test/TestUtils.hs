{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec (Expectation, expectationFailure)

import Types
import AppState

-- | Run an App computation with a specific config and initial state, returning the result.
evalAppTest :: App a -> Maybe AppConfig -> Maybe AppState -> IO a
evalAppTest action mConfig mState =
  fst <$> runAppTest action mConfig mState

-- | Run an App computation with a specific config and initial state, returning the final state.
execAppTest :: App a -> Maybe AppConfig -> Maybe AppState -> IO AppState
execAppTest action mConfig mState =
  snd <$> runAppTest action mConfig mState

-- | Run an App computation with a specific config and initial state, returning both result and final state.
runAppTest :: App a -> Maybe AppConfig -> Maybe AppState -> IO (a, AppState)
runAppTest action mConfig mState = do
  let config = Maybe.fromMaybe defaultConfig mConfig
      state = Maybe.fromMaybe initialState mState
  MonadState.runStateT (MonadReader.runReaderT action config) state

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
optA = createOption "A" "Option A"
optB = createOption "B" "Option B"
optC = createOption "C" "Option C"
optD = createOption "D" "Option D"
optE = createOption "E" "Option E"

allTestOptions :: [Option]
allTestOptions = [optA, optB, optC, optD]

allTestOptionsSet :: Set.Set Option
allTestOptionsSet = Set.fromList allTestOptions

mkAppState :: Set.Set Option -> [(UserId, UserState)] -> AppState
mkAppState options userStatesList = initialState
  { stateOptions = options
  , stateUserStates = Map.fromList userStatesList
  }

mkUserState :: Map.Map OptionId Double -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> UserState
mkUserState ratings prefs violations uncompared = UserState
  { userRatings = ratings
  , userPreferences = prefs
  , userViolations = violations
  , userUncomparedPairs = uncompared
  }

setupStateSingleUser :: UserId -> Set.Set Option -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> AppState
setupStateSingleUser userId options prefs violations uncompared =
  let uState = mkUserState Map.empty prefs violations uncompared
  in mkAppState options [(userId, uState)]

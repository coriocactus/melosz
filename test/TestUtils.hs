{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Test.Hspec (Expectation, shouldSatisfy)

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

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo actual expected =
  shouldSatisfy actual (\x -> abs (x - expected) < epsilon)
  where epsilon = 1e-6

testUser1 :: UserId
testUser1 = "user1"

testUser2 :: UserId
testUser2 = "user2"

optA, optB, optC, optD :: Option
optA = createOption "A" "Option A"
optB = createOption "B" "Option B"
optC = createOption "C" "Option C"
optD = createOption "D" "Option D"

allTestOptions :: [Option]
allTestOptions = [optA, optB, optC, optD]

allTestOptionsSet :: Set.Set Option
allTestOptionsSet = Set.fromList allTestOptions


module SchedulerSpec where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Preference
import Scheduler

import TestUtils

spec :: Spec
spec = describe "Scheduler" $ do
  let setupState user prefs violations uncompared options = initialState
        { stateUsers = Set.singleton user
        , stateOptions = options
        , statePreferences = Map.singleton user prefs
        , stateViolations = Map.singleton user violations
        , stateUncomparedPairs = Map.singleton user uncompared
        }

  describe "getNextComparisonPair" $ do

    it "prefers an uncompared pair if available" $ do
      let uncompared = Set.singleton (makeCanonicalPair optA optB)
          state = setupState testUser1 Set.empty Set.empty uncompared (Set.fromList [optA, optB])
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] Set.empty) Nothing (Just state)
      mPair `shouldBe` Just (makeCanonicalPair optA optB)

    it "prefers a pair from violations if no uncompared pairs exist (stochastic)" $ do
      -- Setup: No uncompared, one violation involving (A,B), (B,C), (C,A)
      let cycleViolation = canonicalizeViolation (optA, optC, optB)
          prefs = Set.fromList [(optA, optB), (optB, optC), (optC, optA)]
          violations = Set.singleton cycleViolation
          violationPairs = findPairsInViolations violations
          state = setupState testUser1 prefs violations Set.empty (Set.fromList [optA, optB, optC])

      -- Test that *some* pair is returned if violations exist
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] violationPairs) Nothing (Just state)
      mPair `shouldSatisfy` (\mp -> maybe False (`Set.member` violationPairs) mp)

    it "returns a pair for refinement if only stable pairs exist" $ do
      -- Setup: A > B, B > C (stable), no uncompared, no violations
      let prefs = Set.fromList [(optA, optB), (optB, optC)]
          state = setupState testUser1 prefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
          ratings = [(optA, 1600), (optB, 1500), (optC, 1400)] -- Provide some ratings

      -- Expectation: Returns *some* pair (maybe (A,B) or (B,C) or (A,C) depending on refinement strategy)
      mPair <- evalAppTest (getNextComparisonPair testUser1 ratings Set.empty) Nothing (Just state)
      mPair `shouldSatisfy` (/= Nothing)

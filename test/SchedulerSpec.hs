module SchedulerSpec where

import qualified Data.Set as Set

import Test.Hspec

import Types
import AppState
import Scheduler

import TestUtils

spec :: Spec
spec = describe "Scheduler" $ do

  let setupState = setupStateSingleUser testUser1 allTestOptionsSet

  describe "getNextComparisonPair" $ do

    it "prefers an uncompared pair if available" $ do
      let uncompared = Set.singleton (makeCanonicalPair optA optB)
          state = setupState Set.empty Set.empty uncompared
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] Set.empty) Nothing (Just state)
      mPair `shouldBe` Just (makeCanonicalPair optA optB)

    it "returns a pair for refinement if only stable pairs exist" $ do
      -- Setup: A > B, B > C (stable), no uncompared, no violations
      let prefs = Set.fromList [(optA, optB), (optB, optC)]
          state = setupState prefs Set.empty Set.empty
          ratings = [(optA, 1600), (optB, 1500), (optC, 1400)]

      -- Expectation: Returns *some* pair
      mPair <- evalAppTest (getNextComparisonPair testUser1 ratings Set.empty) Nothing (Just state)
      mPair `shouldSatisfy` (/= Nothing)

    it "returns Nothing if no pairs are available (e.g., < 2 options)" $ do
      let opts1 = Set.singleton optA
          state = mkAppState opts1 [(testUser1, initialUserState opts1)]
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] Set.empty) Nothing (Just state)
      mPair `shouldBe` Nothing

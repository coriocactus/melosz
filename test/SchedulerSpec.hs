module SchedulerSpec where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Scheduler

import TestUtils

spec :: Spec
spec = describe "Scheduler" $ do

  let setupInitialState user opts prefs viols uncomp =
        let uState = mkUserState Map.empty prefs viols uncomp
        in mkAppState opts [(user, uState)]

  describe "getNextComparisonPair" $ do

    it "prefers an uncompared pair if available" $ do
      let uncompared = Set.singleton (makeCanonicalPair optA optB)
          state = setupInitialState testUser1 allTestOptionsSet Set.empty Set.empty uncompared
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] Set.empty) Nothing state
      mPair `shouldBe` Just (makeCanonicalPair optA optB)

    it "returns a pair for refinement if only stable pairs exist" $ do
      let prefs = Set.fromList [(optA, optB), (optB, optC)]
          state = setupInitialState testUser1 allTestOptionsSet prefs Set.empty Set.empty
          ratings = [(optA, 1600), (optB, 1500), (optC, 1400)]

      mPair <- evalAppTest (getNextComparisonPair testUser1 ratings Set.empty) Nothing state
      mPair `shouldSatisfy` (/= Nothing)

    it "returns Nothing if no pairs are available (e.g., < 2 options)" $ do
      let opts1 = Set.singleton optA
          state = mkAppState opts1 [(testUser1, initialUserState opts1)]
      mPair <- evalAppTest (getNextComparisonPair testUser1 [] Set.empty) Nothing state
      mPair `shouldBe` Nothing

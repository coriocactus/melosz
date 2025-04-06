{-# LANGUAGE OverloadedStrings #-}

module MarshalSpec where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Marshal

import TestUtils

spec :: Spec
spec = describe "Marshal" $ do

  describe "setupOption" $ do
    it "adds a new option to stateOptions" $ do
      finalState <- execAppTest (setupOption optA) Nothing (Just initialState)
      Set.member optA (stateOptions finalState) `shouldBe` True

    it "is idempotent for stateOptions" $ do
      intermediateState <- execAppTest (setupOption optA) Nothing (Just initialState)
      finalState <- execAppTest (setupOption optA) Nothing (Just intermediateState)
      Set.size (stateOptions finalState) `shouldBe` 1

    it "adds new comparison pairs to existing users' UserState" $ do
      -- Setup: User exists with optA already processed
      let initialPairs = getAllOptionPairsSet (Set.singleton optA) -- Should be empty
          startUserState = (initialUserState Set.empty) { userUncomparedPairs = initialPairs }
          startState = mkAppState (Set.singleton optA) [(testUser1, startUserState)]

      -- Action: Add optB
      finalState <- execAppTest (setupOption optB) Nothing (Just startState)

      -- Expectations:
      -- 1. Option B is in global options
      Set.member optB (stateOptions finalState) `shouldBe` True
      -- 2. User1's uncompared pairs now contain (A, B)
      let finalUserState = stateUserStates finalState Map.! testUser1
          uncompared = userUncomparedPairs finalUserState
          expectedPair = makeCanonicalPair optA optB
      Set.member expectedPair uncompared `shouldBe` True
      Set.size uncompared `shouldBe` 1 -- Only the new pair

    it "does not add pairs if no users exist" $ do
      -- Setup: optA exists, no users
      let startState = mkAppState (Set.singleton optA) []
      -- Action: Add optB
      finalState <- execAppTest (setupOption optB) Nothing (Just startState)
      -- Expectation: stateUserStates is still empty
      Map.null (stateUserStates finalState) `shouldBe` True

  describe "setupUser" $ do
    it "adds a new user to stateUserStates" $ do
      finalState <- execAppTest (setupUser testUser1) Nothing (Just initialState)
      Map.member testUser1 (stateUserStates finalState) `shouldBe` True

    it "initializes UserState correctly based on existing options" $ do
      let startState = mkAppState (Set.fromList [optA, optB]) []
          expectedPairs = getAllOptionPairsSet (stateOptions startState)

      finalState <- execAppTest (setupUser testUser1) Nothing (Just startState)
      let userState = stateUserStates finalState Map.! testUser1

      userRatings userState `shouldBe` Map.empty
      userPreferences userState `shouldBe` Set.empty
      userViolations userState `shouldBe` Set.empty
      userUncomparedPairs userState `shouldBe` expectedPairs

    it "is idempotent" $ do
      intermediateState <- execAppTest (setupUser testUser1) Nothing (Just initialState)
      finalState <- execAppTest (setupUser testUser1) Nothing (Just intermediateState)

      stateUserStates finalState `shouldBe` stateUserStates intermediateState
      Map.size (stateUserStates finalState) `shouldBe` 1

  describe "setupOptions" $ do
    it "adds multiple options" $ do
      finalState <- execAppTest (setupOptions [optA, optB]) Nothing (Just initialState)
      Set.size (stateOptions finalState) `shouldBe` 2

  describe "setupUsers" $ do
    it "adds multiple users" $ do
      finalState <- execAppTest (setupUsers [testUser1, testUser2]) Nothing (Just initialState)
      Map.size (stateUserStates finalState) `shouldBe` 2

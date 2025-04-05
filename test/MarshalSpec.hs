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

    it "adds new comparison pairs for existing users" $ do
      -- Setup: User exists, optA exists
      let startState = initialState
            { stateUsers = Set.singleton testUser1
            , stateOptions = Set.singleton optA
            , stateUncomparedPairs = Map.singleton testUser1 Set.empty -- No pairs initially
            }
      -- Action: Add optB
      finalState <- execAppTest (setupOption optB) Nothing (Just startState)
      let uncompared = stateUncomparedPairs finalState Map.! testUser1
          expectedPair = makeCanonicalPair optA optB
      -- Expectation: Pair (A, B) is now in uncompared
      Set.member expectedPair uncompared `shouldBe` True
      Set.size uncompared `shouldBe` 1

    it "does not add pairs if no users exist" $ do
      -- Setup: optA exists, no users
      let startState = initialState { stateOptions = Set.singleton optA }
      -- Action: Add optB
      finalState <- execAppTest (setupOption optB) Nothing (Just startState)
      -- Expectation: stateUncomparedPairs is still empty
      Map.null (stateUncomparedPairs finalState) `shouldBe` True

  describe "setupUser" $ do
    it "adds a new user to stateUsers" $ do
      finalState <- execAppTest (setupUser testUser1) Nothing (Just initialState)
      Set.member testUser1 (stateUsers finalState) `shouldBe` True

    it "initializes preferences, violations for the new user" $ do
      finalState <- execAppTest (setupUser testUser1) Nothing (Just initialState)
      Map.lookup testUser1 (statePreferences finalState) `shouldBe` Just Set.empty
      Map.lookup testUser1 (stateViolations finalState) `shouldBe` Just Set.empty

    it "initializes uncompared pairs based on existing options" $ do
      let startState = initialState { stateOptions = Set.fromList [optA, optB] }
          expectedPairs = getAllOptionPairsSet (stateOptions startState)
      finalState <- execAppTest (setupUser testUser1) Nothing (Just startState)
      Map.lookup testUser1 (stateUncomparedPairs finalState) `shouldBe` Just expectedPairs

    it "is idempotent" $ do
      intermediateState <- execAppTest (setupUser testUser1) Nothing (Just initialState)
      finalState <- execAppTest (setupUser testUser1) Nothing (Just intermediateState)

      statePreferences finalState `shouldBe` statePreferences intermediateState
      stateViolations finalState `shouldBe` stateViolations intermediateState
      stateUncomparedPairs finalState `shouldBe` stateUncomparedPairs intermediateState

      Set.size (stateUsers finalState) `shouldBe` 1

  describe "setupOptions" $ do
    it "adds multiple options" $ do
      finalState <- execAppTest (setupOptions [optA, optB]) Nothing (Just initialState)
      Set.size (stateOptions finalState) `shouldBe` 2

  describe "setupUsers" $ do
    it "adds multiple users" $ do
      finalState <- execAppTest (setupUsers [testUser1, testUser2]) Nothing (Just initialState)
      Set.size (stateUsers finalState) `shouldBe` 2

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
      finalState <- execAppTest (setupOption optA) Nothing initialState
      Set.member optA (stateOptions finalState) `shouldBe` True

    it "is idempotent for stateOptions" $ do
      intermediateState <- execAppTest (setupOption optA) Nothing initialState
      finalState <- execAppTest (setupOption optA) Nothing intermediateState
      Set.size (stateOptions finalState) `shouldBe` 1

    it "adds new comparison pairs and Glicko to existing users' UserState" $ do
      let initialOpts = Set.singleton optA
          startUserState = simpleUserState initialOpts
          startState = mkAppState initialOpts [(testUser1, startUserState)]

      finalState <- execAppTest (setupOption optB) Nothing startState

      Set.member optB (stateOptions finalState) `shouldBe` True
      let finalUserState = stateUserStates finalState Map.! testUser1
          uncompared = userUncomparedPairs finalUserState
          glickos = userGlickos finalUserState
          expectedPair = makeCanonicalPair optA optB

      Set.member expectedPair uncompared `shouldBe` True
      Set.size uncompared `shouldBe` 1

      Map.lookup (optionId optA) glickos `shouldBe` Just initialGlicko
      Map.lookup (optionId optB) glickos `shouldBe` Just initialGlicko
      Map.size glickos `shouldBe` 2

    it "does not add pairs or glickos if no users exist" $ do
      let startState = mkAppState (Set.singleton optA) []
      finalState <- execAppTest (setupOption optB) Nothing startState
      Map.null (stateUserStates finalState) `shouldBe` True

  describe "setupUser" $ do
    it "adds a new user to stateUserStates" $ do
      finalState <- execAppTest (setupUser testUser1) Nothing initialState
      Map.member testUser1 (stateUserStates finalState) `shouldBe` True

    it "initializes UserState correctly based on existing options" $ do
      let startOpts = Set.fromList [optA, optB]
          startState = mkAppState startOpts []
          expectedPairs = getAllOptionPairsSet startOpts
          expectedGlickoMap = Map.fromSet (const initialGlicko) (Set.map optionId startOpts)

      finalState <- execAppTest (setupUser testUser1) Nothing startState
      let userState = stateUserStates finalState Map.! testUser1

      userGlickos userState `shouldBe` expectedGlickoMap
      userPreferences userState `shouldBe` Set.empty
      userViolations userState `shouldBe` Set.empty
      userUncomparedPairs userState `shouldBe` expectedPairs

    it "is idempotent" $ do
      intermediateState <- execAppTest (setupUser testUser1) Nothing initialState
      finalState <- execAppTest (setupUser testUser1) Nothing intermediateState

      stateUserStates finalState `shouldBe` stateUserStates intermediateState
      Map.size (stateUserStates finalState) `shouldBe` 1

  describe "setupOptions" $ do
    it "adds multiple options" $ do
      finalState <- execAppTest (setupOptions [optA, optB]) Nothing initialState
      Set.size (stateOptions finalState) `shouldBe` 2

  describe "setupUsers" $ do
    it "adds multiple users" $ do
      finalState <- execAppTest (setupUsers [testUser1, testUser2]) Nothing initialState
      Map.size (stateUserStates finalState) `shouldBe` 2

{-# LANGUAGE OverloadedStrings #-}

module AppStateSpec where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState

import TestUtils

spec :: Spec
spec = describe "AppState" $ do
  describe "getAllOptionPairsSet" $ do
    it "returns empty set for no options" $
      getAllOptionPairsSet Set.empty `shouldBe` Set.empty

    it "returns empty set for one option" $
      getAllOptionPairsSet (Set.singleton optA) `shouldBe` Set.empty

    it "returns the single pair for two options" $
      getAllOptionPairsSet (Set.fromList [optA, optB]) `shouldBe` Set.singleton (makeCanonicalPair optA optB)

    it "returns all unique pairs for three options" $ do
      let options = Set.fromList [optA, optB, optC]
          expected = Set.fromList
            [ makeCanonicalPair optA optB
            , makeCanonicalPair optA optC
            , makeCanonicalPair optB optC
            ]
      getAllOptionPairsSet options `shouldBe` expected

    it "returns correct number of pairs for N options" $ do
      let options = Set.fromList [optA, optB, optC, optD]
          n = 4
          expectedCount = n * (n - 1) `div` 2
      Set.size (getAllOptionPairsSet options) `shouldBe` expectedCount

  describe "initialUserState" $ do
    it "initializes with default Glickos, empty prefs/violations" $ do
      let uState = initialUserState allTestOptionsSet
          expectedGlickoMap = Map.fromSet (const initialGlicko) (Set.map optionId allTestOptionsSet)
      userGlickos uState `shouldBe` expectedGlickoMap
      userPreferences uState `shouldBe` Set.empty
      userViolations uState `shouldBe` Set.empty

    it "initializes uncompared pairs based on options" $ do
      let uState = initialUserState allTestOptionsSet
          expectedPairs = getAllOptionPairsSet allTestOptionsSet
      userUncomparedPairs uState `shouldBe` expectedPairs

  describe "getUsers" $ do
    it "returns empty set initially" $ do
      users <- evalAppTest getUsers Nothing initialState
      users `shouldBe` Set.empty
    it "returns set of users present in stateUserStates" $ do
      let state = initialState { stateUserStates = Map.fromList [(testUser1, simpleUserState Set.empty), (testUser2, simpleUserState Set.empty)] }
      users <- evalAppTest getUsers Nothing state
      users `shouldBe` Set.fromList [testUser1, testUser2]

  describe "getGlicko'" $ do
    it "returns initial glicko for unknown user/option" $ do
      glicko <- evalAppTest (getGlicko' testUser1 (optionId optA)) Nothing initialState
      glicko `shouldBe` initialGlicko

    it "returns specific glicko when set in user state" $ do
      let customGlicko = initialGlicko { glickoRating = 1600 }
          glickoMap = Map.singleton (optionId optA) customGlicko
          uState = (simpleUserState allTestOptionsSet) { userGlickos = glickoMap }
          state = mkAppState allTestOptionsSet [(testUser1, uState)]
      glicko <- evalAppTest (getGlicko' testUser1 (optionId optA)) Nothing state
      glicko `shouldBe` customGlicko

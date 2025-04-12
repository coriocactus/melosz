{-# LANGUAGE OverloadedStrings #-}

module RatingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Types
import AppState
import Rating

import TestUtils

spec :: Spec
spec = describe "Rating (Glicko-2)" $ do
  let testConfig = defaultTestConfig
      testOpts = Set.fromList [optA, optB, optC]

      mkInitialState :: Map.Map OptionId Glicko -> AppState
      mkInitialState specificGlickoMap =
          let initialUncompared = getAllOptionPairsSet testOpts
              fullGlickoMap = Map.union specificGlickoMap (Map.fromSet (const initialGlicko) (Set.map optionId testOpts))
              uState = mkUserState fullGlickoMap Set.empty Set.empty initialUncompared
          in mkAppState testOpts [(testUser1, uState)]

      pA = initialGlicko { glickoRating = 1600, glickoDeviation = 200, glickoVolatility = 0.05 }
      pB = initialGlicko { glickoRating = 1450, glickoDeviation = 150, glickoVolatility = 0.06 }

  describe "getUserRating" $ do
    it "returns initial Glicko rating for unknown user/option" $ do
      rating <- evalAppTest (getUserRating testUser1 optA) (Just testConfig) initialState
      rating `shouldBe` defaultRating

    it "returns specific Glicko rating when set in user state" $ do
      let state = mkInitialState (Map.singleton (optionId optA) pA)
      rating <- evalAppTest (getUserRating testUser1 optA) (Just testConfig) state
      rating `shouldBe` glickoRating pA

  describe "getUserRatings" $ do
     it "returns initial ratings sorted descending for multiple options" $ do
      let state = mkInitialState Map.empty
      ratings <- evalAppTest (getUserRatings testUser1 [optA, optB, optC]) (Just testConfig) state
      map snd ratings `shouldBe` replicate 3 defaultRating
      Set.fromList (map fst ratings) `shouldBe` testOpts

  describe "updateRatings" $ do
    it "updates both options' glickos after a match (Win for optA)" $ do
      let startState = mkInitialState (Map.fromList [(optionId optA, pA), (optionId optB, pB)])
          pA_initial = pA
          pB_initial = pB

      finalState <- execAppTest (updateRatings testUser1 optA optB Win) (Just testConfig) startState
      let finalGlickos = userGlickos $ stateUserStates finalState Map.! testUser1
          pA_final = finalGlickos Map.! optionId optA
          pB_final = finalGlickos Map.! optionId optB

      glickoRating pA_final `shouldSatisfy` (> glickoRating pA_initial)
      glickoRating pB_final `shouldSatisfy` (< glickoRating pB_initial)

      glickoDeviation pA_final `shouldSatisfy` (>= 0)
      glickoDeviation pB_final `shouldSatisfy` (>= 0)
      glickoVolatility pA_final `shouldSatisfy` (>= 0)
      glickoVolatility pB_final `shouldSatisfy` (>= 0)

    it "updates both options' glickos after a match (Loss for optA)" $ do
      let startState = mkInitialState (Map.fromList [(optionId optA, pA), (optionId optB, pB)])
          pA_initial = pA
          pB_initial = pB

      finalState <- execAppTest (updateRatings testUser1 optA optB Loss) (Just testConfig) startState
      let finalGlickos = userGlickos $ stateUserStates finalState Map.! testUser1
          pA_final = finalGlickos Map.! optionId optA
          pB_final = finalGlickos Map.! optionId optB

      glickoRating pA_final `shouldSatisfy` (< glickoRating pA_initial)
      glickoRating pB_final `shouldSatisfy` (> glickoRating pB_initial)

      glickoDeviation pA_final `shouldSatisfy` (>= 0)
      glickoDeviation pB_final `shouldSatisfy` (>= 0)
      glickoVolatility pA_final `shouldSatisfy` (>= 0)
      glickoVolatility pB_final `shouldSatisfy` (>= 0)

    it "does not modify state if user does not exist" $ do
      let state = initialState
      finalState <- execAppTest (updateRatings testUser1 optA optB Win) (Just testConfig) state
      stateUserStates finalState `shouldBe` Map.empty

  describe "ratingsToMap" $ do
    it "converts a list of (Option, Double) to Map OptionId Double" $ do
      let input = [(optA, 1600.0), (optB, 1500.0)]
          expected = Map.fromList [(optionId optA, 1600.0), (optionId optB, 1500.0)]
      ratingsToMap input `shouldBe` expected

  describe "ratingsToRankings" $ do
    it "converts a sorted list of (Option, Double) to (Option, Int) ranks" $ do
      let input = [(optC, 1650.0), (optA, 1600.0), (optB, 1500.0)]
          expected = [(optC, 1), (optA, 2), (optB, 3)]
      ratingsToRankings input `shouldBe` expected

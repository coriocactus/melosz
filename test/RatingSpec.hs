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
spec = describe "Rating" $ do
  let testConfig = defaultConfig { configInitialRating = 1500.0, configKFactor = 32.0 }
      k = configKFactor testConfig
      initial = configInitialRating testConfig

  describe "resultToScore" $ do
    it "maps Win to 1.0" $
      resultToScore Win `shouldBe` 1.0

    it "maps Loss to 0.0" $
      resultToScore Loss `shouldBe` 0.0

  describe "calculateExpectedScore" $ do
    it "returns 0.5 for equal ratings" $
      calculateExpectedScore 1500.0 1500.0 `shouldBe` 0.5

    it "returns > 0.5 when first rating is higher" $
      calculateExpectedScore 1600.0 1500.0 `shouldSatisfy` (> 0.5)

    it "returns < 0.5 when first rating is lower" $
      calculateExpectedScore 1400.0 1500.0 `shouldSatisfy` (< 0.5)

    it "returns approx 0.64 for 100 point difference" $
      calculateExpectedScore 1600.0 1500.0 `shouldBeCloseTo` (1.0 / (1.0 + 10.0 ** ((-100.0) / 400.0)))

    it "sums to 1.0 when swapping ratings" $
      (calculateExpectedScore 1600.0 1500.0 + calculateExpectedScore 1500.0 1600.0)
        `shouldBeCloseTo` 1.0

  describe "getUserRating" $ do
    it "returns initial rating for unknown user/option" $ do
      rating <- evalAppTest (getUserRating testUser1 optA) (Just testConfig) Nothing
      rating `shouldBe` initial

    it "returns specific rating when set" $ do
      let state = initialState { stateRatings = Map.singleton (testUser1, optionId optA) 1550.0 }
      rating <- evalAppTest (getUserRating testUser1 optA) (Just testConfig) (Just state)
      rating `shouldBe` 1550.0

  describe "getUserRatings" $ do
     it "returns initial ratings sorted descending for multiple options" $ do
      let state = initialState { stateOptions = allTestOptionsSet }
      ratings <- evalAppTest (getUserRatings testUser1 allTestOptions) (Just testConfig) (Just state)
      map snd ratings `shouldBe` replicate (length allTestOptions) initial
      map fst ratings `shouldBe` allTestOptions -- Should retain original order if ratings equal

     it "returns specific ratings sorted descending" $ do
       let ratingsMap = Map.fromList
              [ ((testUser1, optionId optA), 1600.0)
              , ((testUser1, optionId optB), 1500.0)
              , ((testUser1, optionId optC), 1650.0)
              ]
           state = initialState { stateRatings = ratingsMap, stateOptions = Set.fromList [optA, optB, optC] }
       sortedRatings <- evalAppTest (getUserRatings testUser1 [optA, optB, optC]) (Just testConfig) (Just state)
       map fst sortedRatings `shouldBe` [optC, optA, optB]
       map snd sortedRatings `shouldBe` [1650.0, 1600.0, 1500.0]

  describe "updateRatings" $ do
    it "updates both options after a match (Win for optA)" $ do
      let expectedA = calculateExpectedScore initial initial -- 0.5
          expectedB = calculateExpectedScore initial initial -- 0.5
          newRatingA = initial + k * (1.0 - expectedA) -- 1516
          newRatingB = initial + k * (0.0 - expectedB) -- 1484
      finalState <- execAppTest (updateRatings testUser1 optA optB Win) (Just testConfig) (Just initialState)
      let finalRatings = stateRatings finalState

      Map.lookup (testUser1, optionId optA) finalRatings `shouldBe` Just newRatingA
      Map.lookup (testUser1, optionId optB) finalRatings `shouldBe` Just newRatingB

    it "updates both options after a match (Loss for optA)" $ do
      let expectedA = calculateExpectedScore initial initial -- 0.5
          expectedB = calculateExpectedScore initial initial -- 0.5
          newRatingA = initial + k * (0.0 - expectedA) -- 1484
          newRatingB = initial + k * (1.0 - expectedB) -- 1516
      finalState <- execAppTest (updateRatings testUser1 optA optB Loss) (Just testConfig) (Just initialState)
      let finalRatings = stateRatings finalState

      Map.lookup (testUser1, optionId optA) finalRatings `shouldBe` Just newRatingA
      Map.lookup (testUser1, optionId optB) finalRatings `shouldBe` Just newRatingB

  describe "updateRatings" $ do
    it "updates both options after a match (Win for optA)" $ do
      let expectedA = calculateExpectedScore initial initial -- 0.5
          expectedB = calculateExpectedScore initial initial -- 0.5
          newRatingA = initial + k * (1.0 - expectedA) -- 1516
          newRatingB = initial + k * (0.0 - expectedB) -- 1484
      finalState <- execAppTest (updateRatings testUser1 optA optB Win) (Just testConfig) (Just initialState)
      let finalRatings = stateRatings finalState
      Map.lookup (testUser1, optionId optA) finalRatings `shouldBe` Just newRatingA
      Map.lookup (testUser1, optionId optB) finalRatings `shouldBe` Just newRatingB

    it "updates both options after a match (Loss for optA)" $ do
      let expectedA = calculateExpectedScore initial initial -- 0.5
          expectedB = calculateExpectedScore initial initial -- 0.5
          newRatingA = initial + k * (0.0 - expectedA) -- 1484
          newRatingB = initial + k * (1.0 - expectedB) -- 1516
      finalState <- execAppTest (updateRatings testUser1 optA optB Loss) (Just testConfig) (Just initialState)
      let finalRatings = stateRatings finalState
      Map.lookup (testUser1, optionId optA) finalRatings `shouldBe` Just newRatingA
      Map.lookup (testUser1, optionId optB) finalRatings `shouldBe` Just newRatingB

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

{-# LANGUAGE OverloadedStrings #-}

module RankingSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import Ranking

import TestUtils

spec :: Spec
spec = describe "Ranking (SRA)" $ do

  let allOpts = Set.fromList [optA, optB, optC, optD, optE]
      user1 = "User1"; user2 = "User2"; user3 = "User3"
      ranks1 = Map.fromList [(optionId optA, 1), (optionId optB, 2), (optionId optC, 3), (optionId optD, 4), (optionId optE, 5)]
      ranks2 = Map.fromList [(optionId optA, 1), (optionId optC, 2), (optionId optD, 3), (optionId optB, 4), (optionId optE, 5)]
      ranks3 = Map.fromList [(optionId optB, 1), (optionId optA, 2), (optionId optE, 3), (optionId optC, 4), (optionId optD, 5)]
      userRankMaps = Map.fromList [(user1, ranks1), (user2, ranks2), (user3, ranks3)]

  describe "mean" $ do
    it "calculates mean correctly" $ mean [1, 2, 3, 4, 5] `shouldBe` 3.0
    it "handles empty list" $ mean [] `shouldBe` 0.0
    it "handles single element" $ mean [5] `shouldBe` 5.0

  describe "variance" $ do
    -- Var(1, 1, 2) = ( (1-1.33)^2 + (1-1.33)^2 + (2-1.33)^2 ) / (3-1)
    -- = ( (-0.33)^2 + (-0.33)^2 + (0.67)^2 ) / 2
    -- = ( 0.11 + 0.11 + 0.45 ) / 2 = 0.67 / 2 = 0.335
    it "calculates sample variance correctly" $ variance [1, 1, 2] `shouldBeApprox` (1.0/3.0)
    it "returns 0 for less than 2 elements" $ do
      variance [] `shouldBe` 0.0
      variance [5] `shouldBe` 0.0
    it "returns 0 for identical elements" $ variance [3, 3, 3] `shouldBe` 0.0

  describe "ratingsToRankMap" $ do
    it "converts sorted ratings to ranks" $ do
      let ratings = [(optC, 1650.0), (optA, 1600.0), (optB, 1500.0)]
          expected = Map.fromList [(optionId optC, 1), (optionId optA, 2), (optionId optB, 3)]
      ratingsToRankMap ratings `shouldBe` expected

  describe "calculateItemRankVariance" $ do
      it "calculates variance for Item A" $
        -- Ranks for A: 1 (User1), 1 (User2), 2 (User3) -> Mean=1.33, Var=0.333...
        calculateItemRankVariance (optionId optA) (Map.keysSet userRankMaps) userRankMaps `shouldBeApprox` (1.0/3.0)
      it "calculates variance for Item B" $
        -- Ranks for B: 2 (User1), 4 (User2), 1 (User3) -> Mean=2.33, Var=2.333...
        calculateItemRankVariance (optionId optB) (Map.keysSet userRankMaps) userRankMaps `shouldBeApprox` (7.0/3.0)
      it "returns 0 if only one user ranked the item" $ do
        let singleUserMap = Map.singleton user1 ranks1
        calculateItemRankVariance (optionId optA) (Map.keysSet singleUserMap) singleUserMap `shouldBe` 0.0

  describe "calculateCumulativeSetAtDepth" $ do
    it "calculates S_d for d=1" $
      -- Top 1: User1=A, User2=A, User3=B -> {A, B}
      calculateCumulativeSetAtDepth 1 userRankMaps `shouldBe` Set.fromList [optionId optA, optionId optB]
    it "calculates S_d for d=2" $
      -- Top 2: User1={A,B}, User2={A,C}, User3={B,A} -> {A, B, C}
      calculateCumulativeSetAtDepth 2 userRankMaps `shouldBe` Set.fromList [optionId optA, optionId optB, optionId optC]
    it "calculates S_d for d=5 (all items)" $
      calculateCumulativeSetAtDepth 5 userRankMaps `shouldBe` Map.keysSet ranks1 -- All items

  describe "calculateSRA" $ do
    it "matches the goldenMain example results approximately" $ do
      let sraResult = calculateSRA allOpts userRankMaps
          resultLookup = Map.fromList sraResult

      -- Compare with expected values from the paper comment
      resultLookup Map.! 1 `shouldBeApprox` 1.154700
      resultLookup Map.! 2 `shouldBeApprox` 1.105541
      resultLookup Map.! 3 `shouldBeApprox` 1.095445
      resultLookup Map.! 4 `shouldBeApprox` 1.095445
      resultLookup Map.! 5 `shouldBeApprox` 1.095445

    it "returns empty list for fewer than 2 users" $ do
      calculateSRA allOpts (Map.singleton user1 ranks1) `shouldBe` []

    it "returns empty list for no options" $ do
      calculateSRA Set.empty userRankMaps `shouldBe` []

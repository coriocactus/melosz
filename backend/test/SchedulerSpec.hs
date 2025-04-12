{-# LANGUAGE OverloadedStrings #-}

module SchedulerSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Scheduler

import TestUtils

spec :: Spec
spec = describe "Scheduler (MIG)" $ do

  let testOptsSet = Set.fromList [optA, optB, optC]
      testConfigWithOpts = defaultTestConfig { configOptions = testOptsSet }

      initialTestState :: AppState
      initialTestState =
          let glickoMap = Map.fromSet (const initialGlicko) (Set.map optionId testOptsSet)
              uState = mkUserState glickoMap
          in mkAppState testOptsSet [(testUser1, uState)]

  describe "infoValue" $ do
    it "calculates higher info for players with high deviation" $ do
      let p1_high_rd = initialGlicko { glickoDeviation = 300 }
          p2_high_rd = initialGlicko { glickoDeviation = 300 }
          p3_low_rd  = initialGlicko { glickoDeviation = 50 }
      infoValue p1_high_rd p2_high_rd > infoValue p1_high_rd p3_low_rd `shouldBe` True

    it "calculates higher info for players with closer ratings (higher uncertainty)" $ do
      let p1_close = initialGlicko { glickoRating = 1510 }
          p2_close = initialGlicko { glickoRating = 1500 }
          p3_far = initialGlicko { glickoRating = 1800 }
      infoValue p1_close p2_close > infoValue p1_close p3_far `shouldBe` True

  describe "getPairGlickos" $ do
    it "retrieves glickos for a pair" $ do
      let gA = initialGlicko { glickoRating = 1600 }
          gB = initialGlicko { glickoRating = 1400 }
          state = mkAppState testOptsSet [(testUser1, (mkUserState (Map.fromList [(optionId optA, gA), (optionId optB, gB), (optionId optC, initialGlicko)])))]
      mGlickos <- evalAppTest (getPairGlickos testUser1 (optA, optB)) (Just testConfigWithOpts) state
      mGlickos `shouldBe` Just (gA, gB)

    it "returns Nothing if one option's glicko is missing (shouldn't happen with proper init)" $ do
      let gA = initialGlicko { glickoRating = 1600 }
          state = mkAppState testOptsSet [(testUser1, (mkUserState (Map.fromList [(optionId optA, gA)])))]
      mGlickos <- evalAppTest (getPairGlickos testUser1 (optA, optB)) (Just testConfigWithOpts) state
      mGlickos `shouldBe` Nothing

  describe "selectMaxInfoPair" $ do
    it "selects a pair from the set" $ do
      let pairs = getAllOptionPairsSet testOptsSet
      mPair <- evalAppTest (selectMaxInfoPair testUser1 pairs) (Just testConfigWithOpts) initialTestState
      mPair `shouldSatisfy` Maybe.isJust
      Maybe.fromJust mPair `Set.member` pairs `shouldBe` True

    it "returns Nothing for an empty set of pairs" $ do
      mPair <- evalAppTest (selectMaxInfoPair testUser1 Set.empty) (Just testConfigWithOpts) initialTestState
      mPair `shouldBe` Nothing

  describe "getNextComparisonPair" $ do
    it "returns a pair when options exist" $ do
      mPair <- evalAppTest (getNextComparisonPair testUser1) (Just testConfigWithOpts) initialTestState
      mPair `shouldSatisfy` Maybe.isJust
      let (o1, o2) = Maybe.fromJust mPair
      o1 `Set.member` testOptsSet `shouldBe` True
      o2 `Set.member` testOptsSet `shouldBe` True
      optionId o1 /= optionId o2 `shouldBe` True

    it "prioritizes higher information pairs (integration test)" $ do
      let gA = initialGlicko
          gB = initialGlicko
          gC = initialGlicko { glickoDeviation = 350.0 }
          glickoMap = Map.fromList [(optionId optA, gA), (optionId optB, gB), (optionId optC, gC)]
          state = mkAppState testOptsSet [(testUser1, mkUserState glickoMap)]

      mPair <- evalAppTest (getNextComparisonPair testUser1) (Just testConfigWithOpts) state
      let (o1, o2) = Maybe.fromJust mPair
      (o1 == optC || o2 == optC) `shouldBe` True

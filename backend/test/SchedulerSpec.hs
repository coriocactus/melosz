{-# LANGUAGE OverloadedStrings #-}

module SchedulerSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Control.Monad as Monad

import Test.Hspec

import Types
import AppState
import Scheduler

import TestUtils

spec :: Spec
spec = describe "Scheduler (MIG)" $ do

  let testOptsSet = Set.fromList [optA, optB, optC]
      testConfigWithOpts = Just $ defaultTestConfig { configOptions = testOptsSet }
      oidA = optionId optA
      oidB = optionId optB
      oidC = optionId optC

      setupInitialUser :: App ()
      setupInitialUser = ensureStorableUser testUser1 testOptsSet

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
    it "retrieves glickos for a pair after setup" $ do
      let gA = initialGlicko { glickoRating = 1600 }
          gB = initialGlicko { glickoRating = 1400 }
          action = do
            ensureStorableUser testUser1 testOptsSet
            updateStorableRatings testUser1 oidA gA oidC initialGlicko
            updateStorableRatings testUser1 oidB gB oidC initialGlicko
            getPairGlickos testUser1 (optA, optB)

      (resA, resB) <- evalAppTest action testConfigWithOpts
      resA `shouldBe` gA
      resB `shouldBe` gB

    it "retrieves initial glickos if not updated" $ do
        let action = do
              ensureStorableUser testUser1 testOptsSet
              getPairGlickos testUser1 (optA, optB)
        (resA, resB) <- evalAppTest action testConfigWithOpts
        resA `shouldBe` initialGlicko
        resB `shouldBe` initialGlicko

  describe "selectMaxInfoPair" $ do
    it "selects a pair from the set after setup" $ do
      let pairs = getAllOptionPairsSet testOptsSet
          action = do
            setupInitialUser
            selectMaxInfoPair testUser1 pairs
      mPair <- evalAppTest action testConfigWithOpts
      mPair `shouldSatisfy` Maybe.isJust
      Maybe.fromJust mPair `Set.member` pairs `shouldBe` True

    it "returns Nothing for an empty set of pairs" $ do
      mPair <- evalAppTest (setupInitialUser >> selectMaxInfoPair testUser1 Set.empty) testConfigWithOpts
      mPair `shouldBe` Nothing

  describe "getNextComparisonPair" $ do
    it "returns a pair when options exist after setup" $ do
      let action = setupInitialUser >> getNextComparisonPair testUser1
      mPair <- evalAppTest action testConfigWithOpts
      mPair `shouldSatisfy` Maybe.isJust
      let (o1, o2) = Maybe.fromJust mPair
      o1 `Set.member` testOptsSet `shouldBe` True
      o2 `Set.member` testOptsSet `shouldBe` True
      optionId o1 /= optionId o2 `shouldBe` True

    it "prioritizes higher information pairs (integration test)" $ do
      let gC_high_dev = initialGlicko { glickoDeviation = 350.0 } -- Max deviation = max info initially
          action = do
            ensureStorableUser testUser1 testOptsSet
            updateStorableRatings testUser1 oidC gC_high_dev oidA initialGlicko
            getNextComparisonPair testUser1

      results <- Monad.replicateM 10 (evalAppTest action testConfigWithOpts)
      let pairsInvolvingC = filter (\mp -> case mp of Just (o1, o2) -> optionId o1 == oidC || optionId o2 == oidC; Nothing -> False) results

      length pairsInvolvingC `shouldSatisfy` (> 0)
      all (\mp -> Maybe.isJust mp) results `shouldBe` True

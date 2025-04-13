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

  describe "MonadAppState (IORef Handle)" $ do
    let testOptsSet = Set.fromList [optA, optB]
        testConfigWithOpts = Just $ defaultTestConfig { configOptions = testOptsSet }

    describe "getStorableGlicko" $ do
      it "returns initial glicko for unknown user/option before ensureUser" $ do
        glicko <- evalAppTest (getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        glicko `shouldBe` initialGlicko

      it "returns initial glicko after ensureUser" $ do
        glicko <- evalAppTest (ensureStorableUser testUser1 testOptsSet >> getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        glicko `shouldBe` initialGlicko

      it "returns updated glicko after updateStorableRatings" $ do
        let updatedGlicko = initialGlicko { glickoRating = 1600 }
            oidA = optionId optA
            oidB = optionId optB
            setupAction = do
              ensureStorableUser testUser1 testOptsSet
              updateStorableRatings testUser1 oidA updatedGlicko oidB initialGlicko

        glicko <- evalAppTest (setupAction >> getStorableGlicko testUser1 oidA) testConfigWithOpts
        glicko `shouldBe` updatedGlicko

    describe "getAllStorableRatings" $ do
      it "returns empty map for unknown user" $ do
        ratingsMap <- evalAppTest (getAllStorableRatings testUser1) testConfigWithOpts
        ratingsMap `shouldBe` Map.empty

      it "returns initial glickos for all options after ensureUser" $ do
        let expectedMap = Map.fromList [(optionId optA, initialGlicko), (optionId optB, initialGlicko)]
        ratingsMap <- evalAppTest (ensureStorableUser testUser1 testOptsSet >> getAllStorableRatings testUser1) testConfigWithOpts
        ratingsMap `shouldBe` expectedMap

      it "returns updated glickos after updates" $ do
        let glickoA = initialGlicko { glickoRating = 1600 }
            glickoB = initialGlicko { glickoRating = 1400 }
            oidA = optionId optA
            oidB = optionId optB
            setupAction = do
              ensureStorableUser testUser1 testOptsSet
              updateStorableRatings testUser1 oidA glickoA oidB glickoB

        let expectedMap = Map.fromList [(oidA, glickoA), (oidB, glickoB)]
        ratingsMap <- evalAppTest (setupAction >> getAllStorableRatings testUser1) testConfigWithOpts
        ratingsMap `shouldBe` expectedMap

    describe "ensureStorableUser" $ do
      it "makes user known (getStorableGlicko returns initial)" $ do
        glicko <- evalAppTest (ensureStorableUser testUser1 testOptsSet >> getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        glicko `shouldBe` initialGlicko

      it "is idempotent" $ do
        (_, finalState) <- runAppAndGetRefState (ensureStorableUser testUser1 testOptsSet >> ensureStorableUser testUser1 testOptsSet) testConfigWithOpts
        Map.size (stateUserStates finalState) `shouldBe` 1

    describe "setupUser" $ do
      it "ensures user exists (can retrieve initial Glicko afterwards)" $ do
        glicko <- evalAppTest (setupUser testUser1 >> getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        glicko `shouldBe` initialGlicko

      it "initializes UserState based on options in config (checked via getAllStorableRatings)" $ do
        let expectedGlickoMap = Map.fromList [(optionId optA, initialGlicko), (optionId optB, initialGlicko)]
        ratingsMap <- evalAppTest (setupUser testUser1 >> getAllStorableRatings testUser1) testConfigWithOpts
        ratingsMap `shouldBe` expectedGlickoMap

      it "is idempotent (checked via internal state)" $ do
        (_, finalState) <- runAppAndGetRefState (setupUser testUser1 >> setupUser testUser1) testConfigWithOpts
        Map.size (stateUserStates finalState) `shouldBe` 1
        glicko <- evalAppTest (setupUser testUser1 >> setupUser testUser1 >> getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        glicko `shouldBe` initialGlicko

    describe "setupUsers" $ do
      it "adds multiple users (checked via internal state)" $ do
        (_, finalState) <- runAppAndGetRefState (setupUsers [testUser1, testUser2]) testConfigWithOpts
        Map.size (stateUserStates finalState) `shouldBe` 2
        g1 <- evalAppTest (setupUsers [testUser1, testUser2] >> getStorableGlicko testUser1 (optionId optA)) testConfigWithOpts
        g2 <- evalAppTest (setupUsers [testUser1, testUser2] >> getStorableGlicko testUser2 (optionId optB)) testConfigWithOpts
        g1 `shouldBe` initialGlicko
        g2 `shouldBe` initialGlicko


{-# LANGUAGE OverloadedStrings #-}

module RatingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Ord as Ord
import Test.Hspec

import Types
import AppState
import Rating
import Glicko2

import TestUtils

spec :: Spec
spec = describe "Rating" $ do
  let testOptsSet = Set.fromList [optA, optB, optC]
      testConfigWithOpts = Just $ defaultTestConfig { configOptions = testOptsSet, configSystemTau = 0.5 }
      oidA = optionId optA
      oidB = optionId optB
      oidC = optionId optC

      pA_initial = initialGlicko { glickoRating = 1600, glickoDeviation = 200, glickoVolatility = 0.05 }
      pB_initial = initialGlicko { glickoRating = 1450, glickoDeviation = 150, glickoVolatility = 0.06 }

      setupSpecificRatings :: App ()
      setupSpecificRatings = do
        ensureStorableUser testUser1 testOptsSet
        updateStorableRatings testUser1 oidA pA_initial oidC initialGlicko
        updateStorableRatings testUser1 oidB pB_initial oidC initialGlicko

  describe "getUserRatings" $ do
    it "returns initial ratings sorted descending for multiple options after setupUser" $ do
      ratings <- evalAppTest (setupUser testUser1 >> getUserRatings testUser1 [optA, optB, optC]) testConfigWithOpts

      map snd ratings `shouldSatisfy` all (\r -> abs (r - defaultRating) < 1.0)
      Set.fromList (map fst ratings) `shouldBe` testOptsSet

      ratings `shouldBe` List.sortBy (Ord.comparing (Ord.Down . snd)) ratings

    it "returns specific ratings sorted descending after setup" $ do
      ratings <- evalAppTest (setupSpecificRatings >> getUserRatings testUser1 [optA, optB, optC]) testConfigWithOpts
      let ratingMap = Map.fromList $ map (\(opt, r) -> (optionId opt, r)) ratings

      let displayA = glickoRating $ glickoToDisplay pA_initial
      let displayB = glickoRating $ glickoToDisplay pB_initial
      let displayC = glickoRating $ glickoToDisplay initialGlicko

      ratingMap Map.! oidA `shouldBeApprox` displayA
      ratingMap Map.! oidB `shouldBeApprox` displayB
      ratingMap Map.! oidC `shouldBeApprox` displayC

      ratings `shouldBe` List.sortBy (Ord.comparing (Ord.Down . snd)) ratings

  describe "updateRatings" $ do
    it "updates both options' glickos after a match (Win for optA)" $ do
      let action = do
            setupSpecificRatings
            updateRatings testUser1 optA optB Win
            (,) <$> getStorableGlicko testUser1 oidA <*> getStorableGlicko testUser1 oidB

      (pA_final, pB_final) <- evalAppTest action testConfigWithOpts

      glickoRating pA_final `shouldSatisfy` (> glickoRating pA_initial)
      glickoRating pB_final `shouldSatisfy` (< glickoRating pB_initial)

      glickoDeviation pA_final `shouldSatisfy` (\rd -> rd >= 30 && rd <= 350)
      glickoDeviation pB_final `shouldSatisfy` (\rd -> rd >= 30 && rd <= 350)
      glickoVolatility pA_final `shouldSatisfy` (>= 0)
      glickoVolatility pB_final `shouldSatisfy` (>= 0)

    it "updates both options' glickos after a match (Loss for optA)" $ do
      let action = do
            setupSpecificRatings
            updateRatings testUser1 optA optB Loss
            (,) <$> getStorableGlicko testUser1 oidA <*> getStorableGlicko testUser1 oidB

      (pA_final, pB_final) <- evalAppTest action testConfigWithOpts

      glickoRating pA_final `shouldSatisfy` (< glickoRating pA_initial)
      glickoRating pB_final `shouldSatisfy` (> glickoRating pB_initial)

      glickoDeviation pA_final `shouldSatisfy` (\rd -> rd >= 30 && rd <= 350)
      glickoDeviation pB_final `shouldSatisfy` (\rd -> rd >= 30 && rd <= 350)
      glickoVolatility pA_final `shouldSatisfy` (>= 0)
      glickoVolatility pB_final `shouldSatisfy` (>= 0)

    it "does not modify ratings if user does not exist (ensureUser not called)" $ do
      (resMapBefore, stateBefore) <- runAppAndGetRefState (getAllStorableRatings testUser1) testConfigWithOpts
      (resMapAfter, stateAfter) <- runAppAndGetRefState (updateRatings testUser1 optA optB Win >> getAllStorableRatings testUser1) testConfigWithOpts

      resMapBefore `shouldBe` Map.empty
      resMapAfter `shouldBe` Map.empty
      stateBefore `shouldBe` stateAfter

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

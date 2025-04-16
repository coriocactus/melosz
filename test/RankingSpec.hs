{-# LANGUAGE OverloadedStrings #-}

module RankingSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BSC8
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Types
import Ranking

import TestUtils

optA', optB', optC', optD', optE' :: Option
optA' = optA
optB' = optB
optC' = optC
optD' = optD
optE' = optE

oidA, oidB, oidC, oidD, oidE :: OptionId
oidA = optionId optA'
oidB = optionId optB'
oidC = optionId optC'
oidD = optionId optD'
oidE = optionId optE'

allOpts :: [Option]
allOpts = [optA', optB', optC', optD', optE']
allOptsSet :: Set.Set Option
allOptsSet = Set.fromList allOpts
numOpts :: Int
numOpts = Set.size allOptsSet

user1, user2, user3 :: UserId
user1 = testUser1
user2 = testUser2
user3 = "User3"

ranks1 :: RankMap
ranks1 = Map.fromList [(oidA, 1), (oidB, 2), (oidC, 3), (oidD, 4), (oidE, 5)]
ranks2 :: RankMap
ranks2 = Map.fromList [(oidA, 1), (oidC, 2), (oidD, 3), (oidB, 4), (oidE, 5)]
ranks3 :: RankMap
ranks3 = Map.fromList [(oidB, 1), (oidA, 2), (oidE, 3), (oidC, 4), (oidD, 5)]

userRankMaps :: UserRankMaps
userRankMaps = Map.fromList [(user1, ranks1), (user2, ranks2), (user3, ranks3)]

allOpts3 :: [Option]
allOpts3 = [optA', optB', optC']
allOptsSet3 :: Set.Set Option
allOptsSet3 = Set.fromList allOpts3
numOpts3 :: Int
numOpts3 = Set.size allOptsSet3

ranks1_3 :: RankMap
ranks1_3 = Map.fromList [(oidA, 1), (oidB, 2), (oidC, 3)] -- A > B > C
ranks2_3 :: RankMap
ranks2_3 = Map.fromList [(oidA, 1), (oidC, 2), (oidB, 3)] -- A > C > B
ranks3_3 :: RankMap
ranks3_3 = Map.fromList [(oidB, 1), (oidA, 2), (oidC, 3)] -- B > A > C

userRankMaps3 :: UserRankMaps
userRankMaps3 = Map.fromList [(user1, ranks1_3), (user2, ranks2_3), (user3, ranks3_3)]

genRankMap :: Set.Set OptionId -> Gen RankMap
genRankMap opts = do
  let optList = Set.toList opts
  shuffledOpts <- elements (List.permutations optList)
  return $ Map.fromList $ zip shuffledOpts [1..]

arbitraryUserRankMaps :: Gen (Set.Set Option, UserRankMaps)
arbitraryUserRankMaps = do
  let availableOpts = [optA', optB', optC', optD', optE']
  k <- elements [3..5]
  selectedOptsList <- sublistOf availableOpts `suchThat` (\l -> length l == k)
  let selectedOptsSet = Set.fromList selectedOptsList
      selectedOptIds = Set.map optionId selectedOptsSet

  numUsersGen <- elements [2..5]
  userMaps <- sequence $ replicate numUsersGen (genRankMap selectedOptIds)
  let userIdsGen = map (\i -> UserId (BSC8.pack $ "GenUser" <> show i)) [1..numUsersGen]
  return (selectedOptsSet, Map.fromList $ zip userIdsGen userMaps)

rankingsAreComplete :: (Set.Set Option, UserRankMaps) -> Bool
rankingsAreComplete (opts, maps) =
    let optIds = Set.map optionId opts
    in Map.foldl' (\acc rankMap -> acc && Map.keysSet rankMap == optIds) True maps

spec :: Spec
spec = describe "Ranking" $ do

  -- === SRA Tests ===

  describe "mean" $ do
    it "calculates mean correctly" $ mean [1, 2, 3, 4, 5] `shouldBe` 3.0
    it "handles empty list" $ mean [] `shouldBe` 0.0
    it "handles single element" $ mean [5] `shouldBe` 5.0

  describe "variance" $ do
    it "calculates sample variance correctly" $ variance [1, 1, 2] `shouldBeApprox` (1.0/3.0)
    it "returns 0 for less than 2 elements" $ do
      variance [] `shouldBe` 0.0
      variance [5] `shouldBe` 0.0
    it "returns 0 for identical elements" $ variance [3, 3, 3] `shouldBe` 0.0

  describe "ratingsToRankMap" $ do
    it "converts sorted ratings to ranks" $ do
      let ratings = [(optC', 1650.0), (optA', 1600.0), (optB', 1500.0)]
          expected = Map.fromList [(oidC, 1), (oidA, 2), (oidB, 3)]
      ratingsToRankMap ratings `shouldBe` expected

  describe "calculateItemRankVariance" $ do
    it "calculates variance for Item A" $
      calculateItemRankVariance oidA (Map.keysSet userRankMaps) userRankMaps `shouldBeApprox` (1.0/3.0)
    it "calculates variance for Item B" $
      calculateItemRankVariance oidB (Map.keysSet userRankMaps) userRankMaps `shouldBeApprox` (7.0/3.0)
    it "returns 0 if only one user ranked the item" $ do
      let singleUserMap = Map.singleton user1 ranks1
      calculateItemRankVariance oidA (Map.keysSet singleUserMap) singleUserMap `shouldBe` 0.0

  describe "calculateCumulativeSetAtDepth" $ do
    it "calculates S_d for d=1" $
      calculateCumulativeSetAtDepth 1 userRankMaps `shouldBe` Set.fromList [oidA, oidB]
    it "calculates S_d for d=2" $
      calculateCumulativeSetAtDepth 2 userRankMaps `shouldBe` Set.fromList [oidA, oidB, oidC]
    it "calculates S_d for d=5 (all items)" $
      calculateCumulativeSetAtDepth 5 userRankMaps `shouldBe` Map.keysSet ranks1

  describe "SRA Calculation" $ do
    it "matches the goldenMain example results approximately" $ do
      let sraResult = calculateSRA allOptsSet userRankMaps
          resultLookup = Map.fromList sraResult

      resultLookup Map.! 1 `shouldBeApprox` 1.154700
      resultLookup Map.! 2 `shouldBeApprox` 1.105541
      resultLookup Map.! 3 `shouldBeApprox` 1.095445
      resultLookup Map.! 4 `shouldBeApprox` 1.095445
      resultLookup Map.! 5 `shouldBeApprox` 1.095445

    it "returns empty list for fewer than 2 users" $ do
      calculateSRA allOptsSet (Map.singleton user1 ranks1) `shouldBe` []

    it "returns empty list for no options" $ do
      calculateSRA Set.empty userRankMaps `shouldBe` []

  -- === Aggregation Helper Tests ===

  describe "Aggregation Helpers" $ do
    describe "scoresToConsensus" $ do
      it "sorts scores correctly" $ do
        let scores = Map.fromList [(oidA, 10.0), (oidB, 5.0), (oidC, 15.0)]
            expected = [(oidC, 15.0), (oidA, 10.0), (oidB, 5.0)]
        scoresToConsensus scores `shouldBe` expected
      it "handles empty map" $ do
        scoresToConsensus Map.empty `shouldBe` []

    describe "getAllOptionIds" $ do
      it "finds all unique option ids" $ do
        getAllOptionIds userRankMaps `shouldBe` Set.map optionId allOptsSet
      it "handles empty map" $ do
        getAllOptionIds Map.empty `shouldBe` Set.empty

    describe "initializeScores" $ do
      it "initializes scores to 0.0 for all options" $ do
        let expected = Map.fromList [(oidA, 0.0), (oidB, 0.0), (oidC, 0.0), (oidD, 0.0), (oidE, 0.0)]
        initializeScores userRankMaps `shouldBe` expected
      it "handles empty map" $ do
        initializeScores Map.empty `shouldBe` Map.empty

  -- === Aggregation Method Tests ===

  describe "Rank Aggregation Methods" $ do

    describe "aggregateByScoringRule" $ do
      it "correctly applies simple weights [1, 0, 0]" $ do
        let weights = [1.0, 0.0, 0.0]
            expected = [(oidA, 2.0), (oidB, 1.0), (oidC, 0.0)]
        aggregateByScoringRule userRankMaps3 weights `shouldBe` expected

      it "correctly applies Borda-like weights [2, 1, 0]" $ do
        let weights = [2.0, 1.0, 0.0]
            expected = [(oidA, 5.0), (oidB, 3.0), (oidC, 1.0)]
        aggregateByScoringRule userRankMaps3 weights `shouldBe` expected

      it "handles empty userRankMaps" $ do
        aggregateByScoringRule Map.empty [1.0, 0.0] `shouldBe` []

      it "handles weights list shorter than ranks present" $ do
        let weights = [1.0]
            expected = [(oidA, 2.0), (oidB, 1.0), (oidC, 0.0)]
        aggregateByScoringRule userRankMaps3 weights `shouldBe` expected

    describe "aggregatePlurality" $ do
      it "calculates Plurality score correctly for 3 options" $ do
        let expected = [(oidA, 2.0), (oidB, 1.0), (oidC, 0.0)]
        aggregatePlurality numOpts3 userRankMaps3 `shouldBe` expected
      it "calculates Plurality score correctly for 5 options" $ do
        let expected = [(oidA, 2.0), (oidB, 1.0), (oidC, 0.0), (oidD, 0.0), (oidE, 0.0)]
        aggregatePlurality numOpts userRankMaps `shouldBe` expected

    describe "aggregateVeto" $ do
      it "calculates Veto score correctly for 3 options" $ do
        let expected = [(oidA, 3.0), (oidB, 2.0), (oidC, 1.0)]
        aggregateVeto numOpts3 userRankMaps3 `shouldBe` expected
      it "calculates Veto score correctly for 5 options" $ do
        let expected = [(oidA, 3.0), (oidB, 3.0), (oidC, 3.0), (oidD, 2.0), (oidE, 1.0)]
        let result = aggregateVeto numOpts userRankMaps
        result `shouldMatchList` expected

    describe "aggregateBorda" $ do
      it "calculates Borda score correctly for 3 options" $ do
        let expected = [(oidA, 5.0), (oidB, 3.0), (oidC, 1.0)]
        aggregateBorda numOpts3 userRankMaps3 `shouldBe` expected
      it "calculates Borda score correctly for 5 options" $ do
        let expected = [(oidA, 11.0), (oidB, 8.0), (oidC, 6.0), (oidD, 3.0), (oidE, 2.0)]
        aggregateBorda numOpts userRankMaps `shouldBe` expected

    describe "aggregateKemenyYoung" $ do
      let ranksKY1 = Map.fromList [(oidA, 1), (oidB, 2), (oidC, 3)]
          ranksKY2 = Map.fromList [(oidA, 1), (oidB, 2), (oidC, 3)]
          ranksKY3 = Map.fromList [(oidC, 1), (oidB, 2), (oidA, 3)]
          userRankMapsKY = Map.fromList [(user1, ranksKY1), (user2, ranksKY2), (user3, ranksKY3)]
          optionsKY = Set.map optionId allOptsSet3

      it "finds the correct Kemeny-Young ranking (simple case)" $ do
        let expectedRanking = [(oidA, 1.0), (oidB, 2.0), (oidC, 3.0)]
        aggregateKemenyYoung optionsKY userRankMapsKY `shouldBe` expectedRanking

      it "handles unanimous agreement" $ do
        let unanimousMaps = Map.fromList [(user1, ranksKY1), (user2, ranksKY1), (user3, ranksKY1)]
            expectedRanking = [(oidA, 1.0), (oidB, 2.0), (oidC, 3.0)]
        aggregateKemenyYoung optionsKY unanimousMaps `shouldBe` expectedRanking

      it "handles paper example (computationally heavy!)" $ do
        let expectedRankingPaper = [(oidA, 1.0), (oidB, 2.0), (oidC, 3.0), (oidD, 4.0), (oidE, 5.0)]
        aggregateKemenyYoung (Set.map optionId allOptsSet) userRankMaps `shouldBe` expectedRankingPaper

      it "returns empty for no options" $ do
        aggregateKemenyYoung Set.empty userRankMaps `shouldBe` []

      it "returns empty for no users" $ do
        aggregateKemenyYoung optionsKY Map.empty `shouldBe` []

-- === QuickCheck Properties ===

  describe "QuickCheck Properties" $ do
    prop "Plurality score sum equals number of users" $
      forAll arbitraryUserRankMaps $ \(opts, maps) ->
        not (Map.null maps) ==>
          let numUsersGen = fromIntegral $ Map.size maps
              scores = aggregatePlurality (Set.size opts) maps
          in sum (map snd scores) === numUsersGen

    prop "Veto score sum equals numUsers * (numOptions - 1) for complete rankings" $
      forAll (arbitraryUserRankMaps `suchThat` rankingsAreComplete) $ \(opts, maps) ->
        not (Map.null maps) ==>
          let numOptionsGen = Set.size opts
              numUsersGen = fromIntegral $ Map.size maps
              scores = aggregateVeto numOptionsGen maps
          in sum (map snd scores) === numUsersGen * fromIntegral (numOptionsGen - 1)

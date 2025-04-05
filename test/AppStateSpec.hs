{-# LANGUAGE OverloadedStrings #-}

module AppStateSpec where

import qualified Data.Set as Set

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

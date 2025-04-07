{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

import Types

import TestArbs
import TestUtils (optA, optB)

spec :: Spec
spec = describe "Types" $ do
  describe "makeCanonicalPair" $ do
    it "orders options correctly when first ID is smaller" $
      makeCanonicalPair optA optB `shouldBe` (optA, optB)

    it "orders options correctly when second ID is smaller" $
      makeCanonicalPair optB optA `shouldBe` (optA, optB)

    it "handles identical options" $
      makeCanonicalPair optA optA `shouldBe` (optA, optA)

    prop "always puts the option with the smaller ID first" $
      \(TestOption o1, TestOption o2) ->
        let (c1, c2) = makeCanonicalPair o1 o2
        in optionId c1 <= optionId c2

  describe "flipResult" $ do
    it "flips Win to Loss" $
      flipResult Win `shouldBe` Loss

    it "flips Loss to Win" $
      flipResult Loss `shouldBe` Win

    prop "is its own inverse" $
      \(TestMatchResult res) -> flipResult (flipResult res) === res

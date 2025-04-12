{-# LANGUAGE OverloadedStrings #-}

module MarshalSpec where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Marshal

import TestUtils

spec :: Spec
spec = describe "Marshal" $ do

  describe "setupUser" $ do
    it "adds a new user to stateUserStates" $ do
      finalState <- execAppTest (setupUser testUser1) (Just defaultTestConfig) initialState
      Map.member testUser1 (stateUserStates finalState) `shouldBe` True

    it "initializes UserState correctly based on options in config" $ do
      let startOpts = Set.fromList [optA, optB]
          testConfigWithOpts = defaultTestConfig { configOptions = startOpts }
          expectedGlickoMap = Map.fromSet (const initialGlicko) (Set.map optionId startOpts)

      finalState <- execAppTest (setupUser testUser1) (Just testConfigWithOpts) initialState
      let userState = stateUserStates finalState Map.! testUser1

      userGlickos userState `shouldBe` expectedGlickoMap

    it "is idempotent" $ do
      intermediateState <- execAppTest (setupUser testUser1) (Just defaultTestConfig) initialState
      finalState <- execAppTest (setupUser testUser1) (Just defaultTestConfig) intermediateState

      stateUserStates finalState `shouldBe` stateUserStates intermediateState
      Map.size (stateUserStates finalState) `shouldBe` 1

  describe "setupUsers" $ do
    it "adds multiple users" $ do
      finalState <- execAppTest (setupUsers [testUser1, testUser2]) (Just defaultTestConfig) initialState
      Map.size (stateUserStates finalState) `shouldBe` 2

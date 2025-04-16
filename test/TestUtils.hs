{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Control.Monad.Reader as MonadReader
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.IORef as IORef

import Test.Hspec

import Types
import AppState

defaultTestConfig :: AppConfig
defaultTestConfig = AppConfig
  { configSystemTau = 0.5
  , configStateHandle = error "configStateHandle should be set by test runner"
  , configOptions = allTestOptionsSet
  }

evalAppTest :: App a -> Maybe AppConfig -> IO a
evalAppTest action mConfig = do
  ref <- IORef.newIORef initialAppState
  let handle = mkIORefHandle ref
  let config = Maybe.fromMaybe defaultTestConfig mConfig
      finalConfig = config { configStateHandle = handle }
  MonadReader.runReaderT action finalConfig

execAppTest :: App a -> Maybe AppConfig -> IO a
execAppTest action mConfig = evalAppTest action mConfig

runAppAndGetRefState :: App a -> Maybe AppConfig -> IO (a, AppState)
runAppAndGetRefState action mConfig = do
  ref <- IORef.newIORef initialAppState
  let handle = mkIORefHandle ref
  let config = Maybe.fromMaybe defaultTestConfig mConfig
      finalConfig = config { configStateHandle = handle }
  result <- MonadReader.runReaderT action finalConfig
  finalState <- IORef.readIORef ref
  pure (result, finalState)

shouldBeApproxPrec :: (Fractional a, Ord a, Show a) => a -> a -> a -> Expectation
shouldBeApproxPrec margin actual expected =
  if abs (actual - expected) < abs margin * max 1 (abs expected)
    then return ()
    else expectationFailure message
  where
    message = concat [
      "Test Failed\nexpected: ", show expected,
      " within margin of ", show margin,
      "\n but got: ", show actual]

shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldBeApprox = shouldBeApproxPrec 1e-6

testUser1 :: UserId
testUser1 = "user1"

testUser2 :: UserId
testUser2 = "user2"

optA, optB, optC, optD, optE :: Option
optA = createOption "A" "Option A" ""
optB = createOption "B" "Option B" ""
optC = createOption "C" "Option C" ""
optD = createOption "D" "Option D" ""
optE = createOption "E" "Option E" ""

allTestOptions :: [Option]
allTestOptions = [optA, optB, optC, optD]

allTestOptionsSet :: Set.Set Option
allTestOptionsSet = Set.fromList allTestOptions

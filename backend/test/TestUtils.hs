{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Control.Monad.Reader as MonadReader
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef

import Test.Hspec (Expectation, expectationFailure)

import Types
import AppState

data AppConfigData = AppConfigData
  { configDataKFactor :: Double
  , configDataInitialRating :: Double
  }

defaultConfigData :: AppConfigData
defaultConfigData = AppConfigData
  { configDataKFactor = 32.0
  , configDataInitialRating = 1500.0
  }

evalAppTest :: App a -> Maybe AppConfigData -> AppState -> IO a
evalAppTest action mConfigData initState =
  fst <$> runAppTest action mConfigData initState

execAppTest :: App a -> Maybe AppConfigData -> AppState -> IO AppState
execAppTest action mConfigData initState = do
  ref <- IORef.newIORef initState
  (_, finalRef) <- runAppTestRef action mConfigData ref
  IORef.readIORef finalRef

runAppTest :: App a -> Maybe AppConfigData -> AppState -> IO (a, AppState)
runAppTest action mConfigData initState = do
   ref <- IORef.newIORef initState
   (result, finalRef) <- runAppTestRef action mConfigData ref
   finalState <- IORef.readIORef finalRef
   pure (result, finalState)

runAppTestRef :: App a -> Maybe AppConfigData -> IORef.IORef AppState -> IO (a, IORef.IORef AppState)
runAppTestRef action mConfigData stateRef = do
  let configData = Maybe.fromMaybe defaultConfigData mConfigData
      config = AppConfig
        { configKFactor = configDataKFactor configData
        , configInitialRating = configDataInitialRating configData
        , configStateRef = stateRef
        }
  result <- MonadReader.runReaderT action config
  pure (result, stateRef)

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

mkAppState :: Set.Set Option -> [(UserId, UserState)] -> AppState
mkAppState options userStatesList = initialState
  { stateOptions = options
  , stateUserStates = Map.fromList userStatesList
  }

mkUserState :: Map.Map OptionId Double -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> UserState
mkUserState ratings prefs violations uncompared = UserState
  { userRatings = ratings
  , userPreferences = prefs
  , userViolations = violations
  , userUncomparedPairs = uncompared
  }

setupStateSingleUser :: UserId -> Set.Set Option -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> AppState
setupStateSingleUser userId options prefs violations uncompared =
  let uState = mkUserState Map.empty prefs violations uncompared
  in mkAppState options [(userId, uState)]

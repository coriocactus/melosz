module Scheduler where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Random as Random

import Types
import AppState

getNextComparisonPair :: UserId -> [(Option, Double)] -> Set.Set (Option, Option) -> App (Maybe (Option, Option))
getNextComparisonPair uid sortedRatings violationPairs = do
  uncomparedSet <- getUncomparedPairsForUser uid
  violationsSet <- getViolationsForUser uid
  optionsSet <- getOptions

  mUncompared <- selectRandomElement uncomparedSet
  if Maybe.isJust mUncompared then
    pure mUncompared
  else do
    if not (Set.null violationsSet) then do
      let allPairs = getAllOptionPairsSet optionsSet
      let stableComparedPairs = allPairs Set.\\ violationPairs Set.\\ uncomparedSet

      pickViolation <- MonadIO.liftIO $ Random.randomRIO (0.0, 1.0)
      let violationFocusProbability = 0.7 :: Double
      let primaryPool   = if pickViolation < violationFocusProbability then violationPairs else stableComparedPairs
      let secondaryPool = if pickViolation < violationFocusProbability then stableComparedPairs else violationPairs

      mPrimary <- selectRandomElement primaryPool
      case mPrimary of
        Just pair -> pure $ Just pair
        Nothing   -> selectRandomElement secondaryPool
    else do
      let topN = 5
      let topOptionsList = List.take topN $ map fst sortedRatings

      if length topOptionsList < 2 then
         getRandomPairForRefinement optionsSet
      else do
        let topOptionsSet = Set.fromList topOptionsList
        let topPairsSet = getAllOptionPairsSet topOptionsSet
        mTopPair <- selectRandomElement topPairsSet

        case mTopPair of
          Just pair -> pure $ Just pair
          Nothing   -> getRandomPairForRefinement optionsSet

selectRandomElement :: MonadIO.MonadIO m => Set.Set a -> m (Maybe a)
selectRandomElement s
  | Set.null s = pure Nothing
  | otherwise = do
      idx <- MonadIO.liftIO $ Random.randomRIO (0, Set.size s - 1)
      pure $ Just (Set.elemAt idx s)

getRandomPairForRefinement :: MonadIO.MonadIO m => Set.Set Option -> m (Maybe (Option, Option))
getRandomPairForRefinement optionsSet = do
  let n = Set.size optionsSet
  if n < 2
    then pure Nothing
    else do
      idx1 <- MonadIO.liftIO $ Random.randomRIO (0, n - 1)
      idx2 <- MonadIO.liftIO $ pickDifferentIndex n idx1
      let option1 = Set.elemAt idx1 optionsSet
          option2 = Set.elemAt idx2 optionsSet
      pure $ Just (makeCanonicalPair option1 option2)
  where
    pickDifferentIndex :: Int -> Int -> IO Int
    pickDifferentIndex size excludedIndex = do
      idx <- Random.randomRIO (0, size - 2)
      pure $ if idx >= excludedIndex then idx + 1 else idx

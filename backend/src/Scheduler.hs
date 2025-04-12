module Scheduler where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Random as Random

import Types
import AppState
import Glicko2

infoValue :: Glicko -> Glicko -> Double
infoValue p1 p2 = uncertaintyFactor * e * (1 - e) * (g_rd2 ** 2)
  where
    r1 = glickoRating p1
    rd1 = glickoDeviation p1
    r2 = glickoRating p2
    rd2 = glickoDeviation p2
    e = expectedOutcome r1 r2 rd2
    g_rd2 = g rd2
    uncertaintyFactor = rd1 * rd2

getPairGlickos :: UserId -> (Option, Option) -> App (Maybe (Glicko, Glicko))
getPairGlickos uid (opt1, opt2) = do
  mp1 <- getGlicko uid (optionId opt1)
  mp2 <- getGlicko uid (optionId opt2)
  pure $ do
    p1 <- mp1
    p2 <- mp2
    pure (p1, p2)

selectMaxInfoPair :: UserId -> Set.Set (Option, Option) -> App (Maybe (Option, Option))
selectMaxInfoPair _ pairs | Set.null pairs = pure Nothing
selectMaxInfoPair uid pairs = do
  valuedPairs <- mapMaybeM calculatePairValue (Set.toList pairs)
  if null valuedPairs then pure Nothing else
    let maxVal = maximum $ map snd valuedPairs
        maxPairs = map fst $ filter (\(_, val) -> val >= maxVal - 1e-9) valuedPairs
    in if null maxPairs then pure Nothing else Just <$> selectRandomElementList maxPairs
  where
    calculatePairValue :: (Option, Option) -> App (Maybe ((Option, Option), Double))
    calculatePairValue pair = do
      mGlickos <- getPairGlickos uid pair
      case mGlickos of
        Nothing -> pure Nothing
        Just (p1, p2) -> pure $ Just (pair, infoValue p1 p2)

selectRandomElementList :: MonadIO.MonadIO m => [a] -> m a
selectRandomElementList xs = do
  idx <- MonadIO.liftIO $ Random.randomRIO (0, length xs - 1)
  pure $ xs !! idx

getNextComparisonPair :: UserId -> App (Maybe (Option, Option))
getNextComparisonPair uid = do
  optionsSet <- MonadReader.asks configOptions
  let allPairs = getAllOptionPairsSet optionsSet
  selectMaxInfoPair uid allPairs

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap Maybe.catMaybes . Monad.mapM f

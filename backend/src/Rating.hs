module Rating where

import qualified Control.Monad.Reader as MonadReader
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

import Types
import AppState
import Glicko2

getUserRatings :: MonadAppState m => UserId -> [Option] -> m [(Option, Double)]
getUserRatings uid options = do
  glickoMap <- getAllStorableRatings uid
  pure $ List.sortBy (Ord.comparing (Ord.Down . snd)) (ratingsList glickoMap)
  where
    optMap :: Map.Map OptionId Option
    optMap = Map.fromList $ map (\opt -> (optionId opt, opt)) options

    ratingsList :: Map.Map OptionId Glicko -> [(Option, Double)]
    ratingsList glickoMap = Maybe.mapMaybe
      (\ (oid, glicko) -> case Map.lookup oid optMap of
          Just opt -> Just (opt, glickoRating (glickoToDisplay glicko))
          Nothing -> Nothing
      ) (Map.toList glickoMap)

ratingsToMap :: [(Option, Double)] -> Map.Map OptionId Double
ratingsToMap ratings = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) ratings

ratingsToRankings :: [(Option, Double)] -> [(Option, Int)]
ratingsToRankings sortedRatings = zip (map fst sortedRatings) [1..]

updateRatings :: (MonadAppState m, MonadReader.MonadReader AppConfig m) => UserId -> Option -> Option -> MatchResult -> m ()
updateRatings uid option1 option2 result = do
  config <- MonadReader.ask
  let tau = configSystemTau config
      oid1 = optionId option1
      oid2 = optionId option2

  ensureStorableUser uid (configOptions config)

  p1 <- getStorableGlicko uid oid1
  p2 <- getStorableGlicko uid oid2

  let (new_p1, new_p2) = calculateNewRatings p1 p2 result tau

  updateStorableRatings uid oid1 new_p1 oid2 new_p2

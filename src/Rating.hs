module Rating where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Types
import AppState

resultToScore :: MatchResult -> Double
resultToScore Win  = 1.0
resultToScore Loss = 0.0

calculateExpectedScore :: Double -> Double -> Double
calculateExpectedScore aRating bRating =
  1.0 / (1.0 + 10.0 ** ((bRating - aRating) / 400.0))

getUserRating :: UserId -> Option -> App Double
getUserRating uid option =
  Map.findWithDefault
    <$> MonadReader.asks configInitialRating
    <*> pure (uid, optionId option)
    <*> getRatingsMap

getUserRatings :: UserId -> [Option] -> App [(Option, Double)]
getUserRatings uid options =
  List.sortBy (Ord.comparing (Ord.Down . snd)) <$>
    traverse (\opt -> (,) opt <$> getUserRating uid opt) options

ratingsToMap :: [(Option, Double)] -> Map.Map OptionId Double
ratingsToMap ratings =
  Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) ratings

ratingsToRankings :: [(Option, Double)] -> [(Option, Int)]
ratingsToRankings ratings = zip (map fst ratings) [1..]

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings uid option1 option2 result = do
  rating1 <- getUserRating uid option1
  rating2 <- getUserRating uid option2
  config <- getConfig
  let kFactor = configKFactor config

  let expected1 = calculateExpectedScore rating1 rating2
  let expected2 = calculateExpectedScore rating2 rating1

  let (actual1, actual2) = case result of
        Win  -> (1.0, 0.0)
        Loss -> (0.0, 1.0)

  let newRating1 = rating1 + kFactor * (actual1 - expected1)
  let newRating2 = rating2 + kFactor * (actual2 - expected2)

  MonadState.modify $ \s ->
    let updateR1 = Map.insert (uid, optionId option1) newRating1
        updateR2 = Map.insert (uid, optionId option2) newRating2
    in s { stateRatings = updateR2 (updateR1 (stateRatings s)) }

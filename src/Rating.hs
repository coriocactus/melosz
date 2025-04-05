module Rating where

import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Types
import AppState

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

updateRating :: UserId -> Option -> Option -> MatchResult -> App ()
updateRating uid userOption opponentOption result = do
  config <- getConfig
  let kFactor = configKFactor config
  userRating <- getUserRating uid userOption
  opponentRating <- getUserRating uid opponentOption

  let expected = calculateExpectedScore userRating opponentRating
      actual = resultToScore result
      newUserRating = userRating + kFactor * (actual - expected)

  MonadState.modify $ \s -> s
    { stateRatings = Map.insert
        (uid, optionId userOption) newUserRating (stateRatings s)
    }

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings uid option1 option2 result = do
  updateRating uid option1 option2 result
  updateRating uid option2 option1 (flipResult result)


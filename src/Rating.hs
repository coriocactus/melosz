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
getUserRating uid option = do
  initialRating <- MonadReader.asks configInitialRating
  mUserState <- getUserState uid
  pure $ case mUserState of
    Nothing -> initialRating
    Just us -> Map.findWithDefault initialRating (optionId option) (userRatings us)

getUserRatings :: UserId -> [Option] -> App [(Option, Double)]
getUserRatings uid options = do
  ratings <- traverse (\opt -> (,) opt <$> getUserRating uid opt) options
  pure $ List.sortBy (Ord.comparing (Ord.Down . snd)) ratings

ratingsToMap :: [(Option, Double)] -> Map.Map OptionId Double
ratingsToMap ratings =
  Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) ratings

ratingsToRankings :: [(Option, Double)] -> [(Option, Int)]
ratingsToRankings sortedRatings = zip (map fst sortedRatings) [1..]

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings uid option1 option2 result = do
  mUserState <- getUserState uid
  case mUserState of
    Nothing -> pure ()
    Just userState -> do
      config <- getConfig
      let kFactor = configKFactor config
          initialRating = configInitialRating config
          oid1 = optionId option1
          oid2 = optionId option2

      let rating1 = Map.findWithDefault initialRating oid1 (userRatings userState)
          rating2 = Map.findWithDefault initialRating oid2 (userRatings userState)

      let expected1 = calculateExpectedScore rating1 rating2
      let expected2 = calculateExpectedScore rating2 rating1

      let (actual1, actual2) = case result of
            Win  -> (1.0, 0.0)
            Loss -> (0.0, 1.0)

      let newRating1 = rating1 + kFactor * (actual1 - expected1)
      let newRating2 = rating2 + kFactor * (actual2 - expected2)

      let updatedRatings = Map.insert oid1 newRating1 $ Map.insert oid2 newRating2 (userRatings userState)
          updatedUserState = userState { userRatings = updatedRatings }

      MonadState.modify $ \s ->
        s { stateUserStates = Map.insert uid updatedUserState (stateUserStates s) }

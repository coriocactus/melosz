module Rating where

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
  config <- getConfig
  mUserState <- getUserState uid
  pure $ case mUserState of
    Nothing -> configInitialRating config
    Just us -> Map.findWithDefault (configInitialRating config) (optionId option) (userRatings us)

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
  config <- getConfig
  let kFactor = configKFactor config
      initialRating = configInitialRating config
      oid1 = optionId option1
      oid2 = optionId option2

  modifyStateRef_ $ \s ->
    case Map.lookup uid (stateUserStates s) of
      Nothing -> s
      Just userState ->
        let
          rating1 = Map.findWithDefault initialRating oid1 (userRatings userState)
          rating2 = Map.findWithDefault initialRating oid2 (userRatings userState)

          expected1 = calculateExpectedScore rating1 rating2
          expected2 = calculateExpectedScore rating2 rating1

          (actual1, actual2) = case result of
                Win  -> (1.0, 0.0)
                Loss -> (0.0, 1.0)

          newRating1 = rating1 + kFactor * (actual1 - expected1)
          newRating2 = rating2 + kFactor * (actual2 - expected2)

          updatedRatings = Map.insert oid1 newRating1 $ Map.insert oid2 newRating2 (userRatings userState)
          updatedUserState = userState { userRatings = updatedRatings }
          updatedUserStates = Map.insert uid updatedUserState (stateUserStates s)
        in s { stateUserStates = updatedUserStates }

module Rating where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Types
import AppState
import Glicko2

getUserRating :: UserId -> Option -> App Double
getUserRating uid option = glickoRating <$> getGlicko' uid (optionId option)

getUserRatings :: UserId -> [Option] -> App [(Option, Double)]
getUserRatings uid options = do
  glickosWithOpts <- Monad.mapM (\opt -> (,) opt <$> getGlicko' uid (optionId opt)) options
  pure $ List.sortBy (Ord.comparing (Ord.Down . snd)) (mkRatings glickosWithOpts)
  where
    mkRatings :: [(Option, Glicko)] -> [(Option, Double)]
    mkRatings glickoWithOpts = map (\(opt, glicko) -> (opt, glickoRating (glickoToDisplay glicko))) glickoWithOpts

ratingsToMap :: [(Option, Double)] -> Map.Map OptionId Double
ratingsToMap ratings = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) ratings

ratingsToRankings :: [(Option, Double)] -> [(Option, Int)]
ratingsToRankings sortedRatings = zip (map fst sortedRatings) [1..]

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings uid option1 option2 result = do
  config <- getConfig
  let tau = configSystemTau config
      oid1 = optionId option1
      oid2 = optionId option2

  p1 <- getGlicko' uid oid1
  p2 <- getGlicko' uid oid2

  let (new_p1, new_p2) = calculateNewRatings p1 p2 result tau

  modifyStateRef_ $ \s -> case Map.lookup uid (stateUserStates s) of
    Nothing -> s
    Just userState -> s { stateUserStates = updatedUserStates }
      where
        updatedGlickos = Map.insert oid1 new_p1 $ Map.insert oid2 new_p2 (userGlickos userState)
        updatedUserState = userState { userGlickos = updatedGlickos }
        updatedUserStates = Map.insert uid updatedUserState (stateUserStates s)

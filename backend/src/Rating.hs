module Rating where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Types
import AppState
import Glicko2

getUserRating :: UserId -> Option -> App Double
getUserRating uid option = glickoRating <$> getGlickoPlayer' uid (optionId option)

getUserRatings :: UserId -> [Option] -> App [(Option, Double)]
getUserRatings uid options = do
  playersWithOpts <- Monad.mapM (\opt -> (,) opt <$> getGlickoPlayer' uid (optionId opt)) options
  pure $ List.sortBy (Ord.comparing (Ord.Down . snd)) (mkRatings playersWithOpts)
  where
    mkRatings :: [(Option, GlickoPlayer)] -> [(Option, Double)]
    mkRatings glickoWithOpts = map (\(opt, player) -> (opt, glickoRating (glickoToDisplay player))) glickoWithOpts

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

  p1 <- getGlickoPlayer' uid oid1
  p2 <- getGlickoPlayer' uid oid2

  let (new_p1, new_p2) = calculateNewRatings p1 p2 result tau

  modifyStateRef_ $ \s -> case Map.lookup uid (stateUserStates s) of
    Nothing -> s
    Just userState -> s { stateUserStates = updatedUserStates }
      where
        updatedGlickoPlayers = Map.insert oid1 new_p1 $ Map.insert oid2 new_p2 (userGlickoPlayers userState)
        updatedUserState = userState { userGlickoPlayers = updatedGlickoPlayers }
        updatedUserStates = Map.insert uid updatedUserState (stateUserStates s)

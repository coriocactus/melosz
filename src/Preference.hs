module Preference where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Printf as Printf

import Types
import AppState

isPreferred :: Option -> Option -> Relation -> Bool
isPreferred opt1 opt2 rel = Set.member (opt1, opt2) rel

hasBeenCompared :: Option -> Option -> Relation -> Bool
hasBeenCompared opt1 opt2 rel =
  isPreferred opt1 opt2 rel || isPreferred opt2 opt1 rel

canonicalizeViolation :: (Option, Option, Option) -> (Option, Option, Option)
canonicalizeViolation (a, c, b)
  | idA <= idB && idA <= idC = (a, c, b)
  | idB <  idA && idB <= idC = (b, a, c)
  | otherwise                = (c, b, a)
  where
    idA = optionId a
    idB = optionId b
    idC = optionId c

checkIfComplete :: UserId -> App Bool
checkIfComplete userId = do
  mUserState <- getUserState userId
  case mUserState of
    Nothing -> pure True
    Just us -> pure $ Set.null (userUncomparedPairs us) && Set.null (userViolations us)

calculateTransitivityScore :: UserId -> App Double
calculateTransitivityScore userId = do
  optionsCount <- Set.size <$> getOptions
  violationsCount <- Set.size <$> getViolationsForUser userId

  pure $ transitivityScore optionsCount violationsCount
  where
    transitivityScore :: Int -> Int -> Double
    transitivityScore n violationsCnt
      | n < 3 = 1.0
      | violationsCnt == 0 = 1.0
      | otherwise = 1.0 - (fromIntegral violationsCnt / totalPossibleTriples n)

    totalPossibleTriples :: Int -> Double
    totalPossibleTriples n
      | n < 3 = 1.0
      | otherwise = fromIntegral $ n * (n - 1) * (n - 2) `div` 6

calculateAgreementScore :: UserId -> [(Option, Double)] -> App Double
calculateAgreementScore userId eloRatingsList = do
  let eloRatingsMap = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) eloRatingsList

  userPrefs <- getPreferencesForUser userId
  let comparedPairs = Set.toList userPrefs
      totalCompared = length comparedPairs

  if totalCompared == 0
    then pure 1.0
    else do
      initialRating <- MonadReader.asks configInitialRating
      let (concordant, discordant) = foldl (countAgreement eloRatingsMap initialRating) (0, 0) comparedPairs
      let tau = if totalCompared == 0 then 0 else (fromIntegral concordant - fromIntegral discordant) / fromIntegral totalCompared
      pure $ (tau + 1.0) / 2.0
  where
    countAgreement :: Map.Map OptionId Double -> Double -> (Int, Int) -> (Option, Option) -> (Int, Int)
    countAgreement eloMap defaultRating (conc, disc) (preferredOption, nonPreferredOption) =
      let prefId = optionId preferredOption
          nonPrefId = optionId nonPreferredOption
          lookupRating oid = Map.findWithDefault defaultRating oid eloMap

          ratingPref = lookupRating prefId
          ratingNonPref = lookupRating nonPrefId
      in
      if ratingPref > ratingNonPref then (conc + 1, disc)
      else if ratingPref < ratingNonPref then (conc, disc + 1)
      else (conc, disc)

findPairsInViolations :: Set.Set (Option, Option, Option) -> Set.Set (Option, Option)
findPairsInViolations violations =
  Set.foldl' addPairsFromTriple Set.empty violations
  where
    addPairsFromTriple :: Set.Set (Option, Option) -> (Option, Option, Option) -> Set.Set (Option, Option)
    addPairsFromTriple acc (a, c, b) =
      let p1 = makeCanonicalPair a b
          p2 = makeCanonicalPair b c
          p3 = makeCanonicalPair c a
      in Set.insert p1 (Set.insert p2 (Set.insert p3 acc))

findCurrentViolations :: Set.Set Option -> Relation -> Set.Set (Option, Option, Option)
findCurrentViolations optionsSet prefs = Set.fromList $ do
  a <- Set.toList optionsSet
  b <- Set.toList optionsSet
  Monad.guard (optionId a < optionId b)
  c <- Set.toList optionsSet
  Monad.guard (optionId b < optionId c)

  let cycle1 = isPreferred a b prefs && isPreferred b c prefs && isPreferred c a prefs
  let cycle2 = isPreferred a c prefs && isPreferred c b prefs && isPreferred b a prefs

  Monad.guard (cycle1 || cycle2)
  pure $ canonicalizeViolation (a, c, b)

calculateUpdatedViolations :: Set.Set Option -> Relation -> Set.Set (Option, Option, Option)
calculateUpdatedViolations optionsSet currentPrefs =
  findCurrentViolations optionsSet currentPrefs

recordComparison :: UserId -> Option -> Option -> MatchResult -> App ()
recordComparison userId opt1 opt2 result = do
  mUserState <- getUserState userId
  case mUserState of
    Nothing -> MonadIO.liftIO $ Printf.printf "Error: User %s not found when recording comparison.\n" (show userId)
    Just userState -> do
      timestamp <- getNextTimestamp

      let (winner, loser) = case result of
            Win -> (opt1, opt2)
            Loss -> (opt2, opt1)
          newPref = (winner, loser)
          reversePref = (loser, winner)
          canonicalPair = makeCanonicalPair opt1 opt2

      let previousPrefs = userPreferences userState
          reversePrefExisted = Set.member reversePref previousPrefs

      Monad.when reversePrefExisted $
        MonadIO.liftIO $ putStrLn $
          "Reversal detected for user " ++ show userId ++ ": Previously preferred " ++ show (optionName loser) ++ " over " ++ show (optionName winner) ++ ", now reversed."

      let userPrefsWithoutReverse = Set.delete reversePref previousPrefs
          newUserPrefs = Set.insert newPref userPrefsWithoutReverse

      let newUserUncompared = Set.delete canonicalPair (userUncomparedPairs userState)

      optionsSet <- getOptions
      let oldViolationCount = Set.size $ userViolations userState
          newUserViolations = calculateUpdatedViolations optionsSet newUserPrefs
          newViolationCount = Set.size newUserViolations

      Monad.when (newViolationCount /= oldViolationCount) $
        MonadIO.liftIO $ Printf.printf "Violations update for %s: %d -> %d. Total: %d\n" (show userId) oldViolationCount newViolationCount newViolationCount

      let updatedUserState = userState
            { userPreferences = newUserPrefs
            , userUncomparedPairs = newUserUncompared
            , userViolations = newUserViolations
            , userRatings = userRatings userState
            }

      MonadState.modify $ \s ->
        s { stateUserStates = Map.insert userId updatedUserState (stateUserStates s)
          , stateNextTimestamp = timestamp + 1
          }

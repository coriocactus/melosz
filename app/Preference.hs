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

referencesEdge :: Option -> Option -> (Option, Option, Option) -> Bool
referencesEdge u v (a, c, b) =
    (a == u && b == v) || (b == u && c == v) || (c == u && a == v)

checkIfComplete :: UserId -> App Bool
checkIfComplete userId = do
  uncomparedSet <- getUncomparedPairsForUser userId
  violationsSet <- getViolationsForUser userId
  pure $ Set.null uncomparedSet && Set.null violationsSet

calculateTransitivityScore :: UserId -> App Double
calculateTransitivityScore userId = do
  options <- MonadState.gets (Set.toList . stateOptions)
  violationsSet <- getViolationsForUser userId

  pure $ transitivityScore (length options) (Set.size violationsSet)
  where
    transitivityScore :: Int -> Int -> Double
    transitivityScore n violationsCount
      | n < 3 = 1.0
      | violationsCount == 0 = 1.0
      | otherwise = 1.0 - (fromIntegral violationsCount / totalPossibleTriples n)

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

updateViolationsOnNewPreference :: UserId -> (Option, Option) -> Maybe (Option, Option) -> App ()
updateViolationsOnNewPreference userId (winner, loser) mRemovedPref = do
  prefsMap <- MonadState.gets statePreferences
  optionsSet <- getOptions
  let userPrefs = Map.findWithDefault Set.empty userId prefsMap

  currentViolations <- getViolationsForUser userId

  let (violationsAfterRemoval, removedCount) = case mRemovedPref of
        Nothing -> (currentViolations, 0)
        Just (rLoser, rWinner) ->
          let (removedCycles, remainingCycles) = Set.partition (referencesEdge rLoser rWinner) currentViolations
          in (remainingCycles, Set.size removedCycles)

  let newViolations = Set.fromList $ do
        x <- Set.toList optionsSet
        Monad.guard (x /= winner && x /= loser)
        Monad.guard (isPreferred loser x userPrefs && isPreferred x winner userPrefs)
        pure $ canonicalizeViolation (winner, x, loser)

  let finalViolations = Set.union violationsAfterRemoval newViolations
      addedCount = Set.size newViolations

  MonadState.modify $ \s -> s { stateViolations = Map.insert userId finalViolations (stateViolations s) }

  Monad.unless (addedCount == 0 && removedCount == 0) $
    MonadIO.liftIO $ Printf.printf "Violations update for %s: +%d, -%d. Total: %d\n" (show userId) addedCount removedCount (Set.size finalViolations)

recordComparison :: UserId -> Option -> Option -> MatchResult -> App ()
recordComparison userId opt1 opt2 result = do
  timestamp <- getNextTimestamp

  -- let comparison = Comparison userId opt1 opt2 result timestamp
  -- MonadState.liftIO $ putStrLn $ "Logged:" <> show comparison

  let (winner, loser) = case result of
        Win -> (opt1, opt2)
        Loss -> (opt2, opt1)
      newPref = (winner, loser)
      reversePref = (loser, winner)
      canonicalPair = makeCanonicalPair opt1 opt2

  currentUserPrefs <- getPreferencesForUser userId
  let reversePrefExisted = Set.member reversePref currentUserPrefs

  Monad.when reversePrefExisted $
    MonadIO.liftIO $ putStrLn $
      "Reversal detected for user " ++ show userId ++ ": Previously preferred " ++ show (optionName loser) ++ " over " ++ show (optionName winner) ++ ", now reversed."

  let userPrefsWithoutReverse = Set.delete reversePref currentUserPrefs
      newUserPrefs = Set.insert newPref userPrefsWithoutReverse

  currentUserUncompared <- getUncomparedPairsForUser userId
  let newUserUncompared = Set.delete canonicalPair currentUserUncompared

  MonadState.modify $ \s ->
    s { statePreferences = Map.insert userId newUserPrefs (statePreferences s)
      , stateUncomparedPairs = Map.insert userId newUserUncompared (stateUncomparedPairs s)
      , stateNextTimestamp = timestamp + 1 -- Increment timestamp for the next event
      }

  updateViolationsOnNewPreference userId newPref (if reversePrefExisted then Just reversePref else Nothing)

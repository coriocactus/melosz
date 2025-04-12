module Preference where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
calculateAgreementScore userId _eloRatingsList = do
  userPrefs <- getPreferencesForUser userId
  let comparedPairs = Set.toList userPrefs
      totalCompared = length comparedPairs

  if totalCompared == 0
    then pure 1.0
    else do
      let optionIdsInPrefs = Set.unions $ map (\(o1, o2) -> Set.fromList [optionId o1, optionId o2]) comparedPairs
      glickosMap <- fmap Map.fromList $ Monad.forM (Set.toList optionIdsInPrefs) $ \oid -> do
          glicko <- getGlicko' userId oid
          pure (oid, glicko)

      let (concordant, discordant) = foldl (countAgreement glickosMap) (0, 0) comparedPairs
      let tau = if totalCompared == 0 then 0 else (fromIntegral concordant - fromIntegral discordant) / fromIntegral totalCompared
      pure $ (tau + 1.0) / 2.0
  where
    countAgreement :: Map.Map OptionId Glicko -> (Int, Int) -> (Option, Option) -> (Int, Int)
    countAgreement glickoMap (conc, disc) (preferredOption, nonPreferredOption) =
      let prefId = optionId preferredOption
          nonPrefId = optionId nonPreferredOption
          lookupGlicko oid = Map.findWithDefault initialGlicko oid glickoMap

          glickoPref = lookupGlicko prefId
          glickoNonPref = lookupGlicko nonPrefId

          ratingPref = glickoRating glickoPref
          ratingNonPref = glickoRating glickoNonPref
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
  optionsSet <- getOptions

  modifyStateRef_ $ \s -> case Map.lookup userId (stateUserStates s) of
    Nothing -> s
    Just userState ->
      let
        (winner, loser) = case result of
              Win -> (opt1, opt2)
              Loss -> (opt2, opt1)
        newPref = (winner, loser)
        reversePref = (loser, winner)
        canonicalPair = makeCanonicalPair opt1 opt2

        previousPrefs = userPreferences userState

        userPrefsWithoutReverse = Set.delete reversePref previousPrefs
        newUserPrefs = Set.insert newPref userPrefsWithoutReverse

        newUserUncompared = Set.delete canonicalPair (userUncomparedPairs userState)

        newUserViolations = calculateUpdatedViolations optionsSet newUserPrefs

        updatedUserState = userState
          { userPreferences = newUserPrefs
          , userUncomparedPairs = newUserUncompared
          , userViolations = newUserViolations
          }

        updatedUserStates = Map.insert userId updatedUserState (stateUserStates s)
      in s { stateUserStates = updatedUserStates }

restoreUserState :: UserId -> Relation -> App ()
restoreUserState userId restoredPrefs = do
  optionsSet <- getOptions
  let allPossiblePairs = getAllOptionPairsSet optionsSet
      comparedCanonicalPairs = Set.map (\(winner, loser) -> makeCanonicalPair winner loser) restoredPrefs
      newUserUncomparedPairs = allPossiblePairs Set.\\ comparedCanonicalPairs
      newUserViolations = calculateUpdatedViolations optionsSet restoredPrefs

      allOptionIds = Set.map optionId optionsSet
      initialGlickos = Map.fromSet (const initialGlicko) allOptionIds

      newUserState = UserState
        { userGlickos = initialGlickos
        , userPreferences = restoredPrefs
        , userViolations = newUserViolations
        , userUncomparedPairs = newUserUncomparedPairs
        }

  modifyStateRef_ $ \s ->
      s { stateUserStates = Map.insert userId newUserState (stateUserStates s) }

restoreUserStateWithGlickos :: UserId -> Relation -> Map.Map OptionId Glicko -> App ()
restoreUserStateWithGlickos userId restoredPrefs restoredGlickos = do
  optionsSet <- getOptions
  let allPossiblePairs = getAllOptionPairsSet optionsSet
      comparedCanonicalPairs = Set.map (\(winner, loser) -> makeCanonicalPair winner loser) restoredPrefs
      newUserUncomparedPairs = allPossiblePairs Set.\\ comparedCanonicalPairs
      newUserViolations = calculateUpdatedViolations optionsSet restoredPrefs

      allOptionIds = Set.map optionId optionsSet
      finalGlickos = Map.union restoredGlickos (Map.fromSet (const initialGlicko) allOptionIds)


      newUserState = UserState
        { userGlickos = finalGlickos
        , userPreferences = restoredPrefs
        , userViolations = newUserViolations
        , userUncomparedPairs = newUserUncomparedPairs
        }

  modifyStateRef_ $ \s ->
    s { stateUserStates = Map.insert userId newUserState (stateUserStates s) }

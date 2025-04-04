{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.String as String
import qualified System.Random as Random
import qualified Text.Printf as Printf

newtype UserId = UserId BS.ByteString
  deriving (Show, Eq, Ord)

instance String.IsString UserId where
  fromString s = UserId (String.fromString s)

newtype OptionId = OptionId BS.ByteString
  deriving (Show, Eq, Ord)

instance String.IsString OptionId where
  fromString s = OptionId (String.fromString s)

data Option = Option
  { optionId :: OptionId
  , optionName :: BS.ByteString
  } deriving (Show, Eq, Ord)

type Relation = Set.Set (Option, Option)

data MatchResult = Win | Loss
  deriving (Show, Eq)

data Comparison = Comparison
  { compUser :: UserId
  , compOption1 :: Option
  , compOption2 :: Option
  , compResult :: MatchResult
  , compTimestamp :: Int
  } deriving (Show, Eq)

type App = MonadReader.ReaderT AppConfig (MonadState.StateT AppState IO)

data AppConfig = AppConfig
  { configKFactor :: Double
  , configInitialRating :: Double
  }

data AppState = AppState
  { stateUsers :: Set.Set UserId
  , stateOptions :: Set.Set Option
  , stateRatings :: Map.Map (UserId, OptionId) Double
  , statePreferences :: Map.Map UserId Relation
  , stateNextTimestamp :: Int
  , stateViolations :: Map.Map UserId (Set.Set (Option, Option, Option))
  , stateUncomparedPairs :: Map.Map UserId (Set.Set (Option, Option))
  }

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configKFactor = 32.0
  , configInitialRating = 1500.0
  }

initialState :: AppState
initialState = AppState
  { stateUsers = Set.empty
  , stateOptions = Set.empty
  , stateRatings = Map.empty
  , statePreferences = Map.empty
  , stateNextTimestamp = 0
  , stateViolations = Map.empty
  , stateUncomparedPairs = Map.empty
  }

makeCanonicalPair :: Option -> Option -> (Option, Option)
makeCanonicalPair o1 o2
  | optionId o1 <= optionId o2 = (o1, o2)
  | otherwise                  = (o2, o1)

createOption :: OptionId -> BS.ByteString -> Option
createOption oid name = Option oid name

createUser :: BS.ByteString -> UserId
createUser uid = UserId uid

addOption :: Option -> App ()
addOption newOption = do
  alreadyExists <- MonadState.gets (Set.member newOption . stateOptions)
  Monad.unless alreadyExists $ do
    existingOptions <- MonadState.gets stateOptions
    userIds <- MonadState.gets stateUsers

    MonadState.modify $ \s -> s { stateOptions = Set.insert newOption existingOptions }

    let pairsToAdd = Set.map (makeCanonicalPair newOption) existingOptions
    MonadState.modify $ \s ->
      let updateUncompared u currentMap =
            Map.insertWith Set.union u pairsToAdd currentMap
      in s { stateUncomparedPairs = Set.foldr updateUncompared (stateUncomparedPairs s) userIds }

addUser :: UserId -> App ()
addUser userId = do
  alreadyExists <- MonadState.gets (Set.member userId . stateUsers)
  Monad.unless alreadyExists $ do
    optionsSet <- MonadState.gets stateOptions

    MonadState.modify $ \s -> s
      { stateUsers = Set.insert userId (stateUsers s)
      , statePreferences = Map.insert userId Set.empty (statePreferences s)
      , stateViolations = Map.insert userId Set.empty (stateViolations s)
      , stateUncomparedPairs = Map.insert userId (getAllOptionPairsSet optionsSet) (stateUncomparedPairs s)
      }

getRating :: UserId -> Option -> App Double
getRating uid option = do
  ratings <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating
  pure $ Map.findWithDefault initialRating (uid, optionId option) ratings

getUserRatings :: UserId -> App [(Option, Double)]
getUserRatings userId = do
  optionsSet <- MonadState.gets stateOptions
  ratingsMap <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating

  let ratingsList = map (getOptionRating ratingsMap initialRating) (Set.toList optionsSet)

  pure $ List.sortBy (Ord.comparing (Ord.Down . snd)) ratingsList
  where
    getOptionRating :: Map.Map (UserId, OptionId) Double -> Double -> Option -> (Option, Double)
    getOptionRating ratings initialRating option =
      let rating = Map.findWithDefault initialRating (userId, optionId option) ratings
      in (option, rating)

updateRating :: UserId -> Option -> Option -> MatchResult -> App ()
updateRating userId option opponent result = do
  kFactor <- MonadReader.asks configKFactor
  optionRating <- getRating userId option
  opponentRating <- getRating userId opponent

  let expected = calculateExpectedScore optionRating opponentRating
      actual = resultToScore result
      newRating = optionRating + kFactor * (actual - expected)

  MonadState.modify $ \s -> s
    { stateRatings = Map.insert
        (userId, optionId option) newRating (stateRatings s)
    }

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings userId option1 option2 result = do
  updateRating userId option1 option2 result
  updateRating userId option2 option1 (flipResult result)

calculateExpectedScore :: Double -> Double -> Double
calculateExpectedScore aRating bRating =
  1.0 / (1.0 + 10.0 ** ((bRating - aRating) / 400.0))

resultToScore :: MatchResult -> Double
resultToScore Win  = 1.0
resultToScore Loss = 0.0

flipResult :: MatchResult -> MatchResult
flipResult Win  = Loss
flipResult Loss = Win

getAllOptionPairs :: [Option] -> [(Option, Option)]
getAllOptionPairs options = do
  opt1 <- options
  opt2 <- options
  Monad.guard (optionId opt1 < optionId opt2)
  pure (opt1, opt2)

getAllOptionPairsSet :: Set.Set Option -> Set.Set (Option, Option)
getAllOptionPairsSet options = Set.fromList $ do
  o1 <- Set.toList options
  o2 <- Set.toList options
  Monad.guard (optionId o1 < optionId o2)
  pure (o1, o2)

isPreferred :: Option -> Option -> Relation -> Bool
isPreferred opt1 opt2 rel = Set.member (opt1, opt2) rel

hasBeenCompared :: Option -> Option -> Relation -> Bool
hasBeenCompared opt1 opt2 rel =
  isPreferred opt1 opt2 rel || isPreferred opt2 opt1 rel

checkIfComplete :: UserId -> App Bool
checkIfComplete userId = do
  uncomparedSet <- MonadState.gets (Map.findWithDefault Set.empty userId . stateUncomparedPairs)
  violations <- getViolations userId
  pure $ Set.null uncomparedSet && Set.null violations

getNextComparisonPair :: UserId -> App (Maybe (Option, Option))
getNextComparisonPair userId = do
  uncomparedSet <- MonadState.gets (Map.findWithDefault Set.empty userId . stateUncomparedPairs)
  violations <- getViolations userId
  isDone <- checkIfComplete userId

  mUncompared <- selectRandomElement uncomparedSet

  if Maybe.isJust mUncompared then pure mUncompared else do
    let violationPairs = findPairsInViolations violations
    mViolationPair <- selectRandomElement violationPairs

    if Maybe.isJust mViolationPair then pure mViolationPair else do
      if isDone
         then pure Nothing
         else getRandomPairForRefinement userId

findPairsInViolations :: Set.Set (Option, Option, Option) -> Set.Set (Option, Option)
findPairsInViolations violations =
  Set.foldl' addPairsFromTriple Set.empty violations
  where
    addPairsFromTriple :: Set.Set (Option, Option) -> (Option, Option, Option) -> Set.Set (Option, Option)
    addPairsFromTriple acc (a, c, b) =
      let p1 = makeCanonicalPair a b
          p2 = makeCanonicalPair b c
          p3 = makeCanonicalPair a c
      in Set.insert p1 (Set.insert p2 (Set.insert p3 acc))

getViolations :: UserId -> App (Set.Set (Option, Option, Option))
getViolations userId =
  MonadState.gets (Map.findWithDefault Set.empty userId . stateViolations)

findMinimalTransitivityViolations :: UserId -> App [(Option, Option)]
findMinimalTransitivityViolations userId = do
  violations <- getViolations userId
  if Set.null violations
    then pure []
    else do
      let triples = Set.toList violations
      let pairFrequencies = Map.toList $ foldl countViolations Map.empty triples
      let sortedPairs = List.sortBy (\(_, freq1) (_, freq2) -> compare freq2 freq1) pairFrequencies

      pure $ map fst sortedPairs
  where
    countViolations :: Map.Map (Option, Option) Int -> (Option, Option, Option) -> Map.Map (Option, Option) Int
    countViolations freqMap (a, b, c) =
      let pairs = [makeCanonicalPair a b, makeCanonicalPair b c, makeCanonicalPair a c]
          updateFreq m p = Map.insertWith (+) p 1 m
      in foldl updateFreq freqMap pairs

selectRandomElement :: MonadState.MonadIO m => Set.Set a -> m (Maybe a)
selectRandomElement s
  | Set.null s = pure Nothing
  | otherwise = do
      let n = Set.size s
      idx <- MonadState.liftIO $ Random.randomRIO (0, n - 1)
      pure $ Just (Set.elemAt idx s)

getRandomPairForRefinement :: UserId -> App (Maybe (Option, Option))
getRandomPairForRefinement _ = do
  optionsSet <- MonadState.gets stateOptions
  let n = Set.size optionsSet
  if n < 2
    then pure Nothing
    else do
      idx1 <- MonadState.liftIO $ Random.randomRIO (0, n - 1)
      idx2 <- MonadState.liftIO $ pickDifferentIndex n idx1
      let option1 = Set.elemAt idx1 optionsSet
          option2 = Set.elemAt idx2 optionsSet
      pure $ Just (makeCanonicalPair option1 option2)
  where
    pickDifferentIndex :: Int -> Int -> IO Int
    pickDifferentIndex size excludedIndex = do
      idx <- Random.randomRIO (0, size - 2)
      pure $ if idx >= excludedIndex then idx + 1 else idx

recordComparison :: UserId -> Option -> Option -> MatchResult -> App ()
recordComparison userId opt1 opt2 result = do
  timestamp <- MonadState.gets stateNextTimestamp

  let comparison = Comparison userId opt1 opt2 result timestamp
  MonadState.liftIO $ putStrLn $ "Logged:" <> show comparison

  let (winner, loser) = case result of
        Win -> (opt1, opt2)
        Loss -> (opt2, opt1)
      newPref = (winner, loser)
      reversePref = (loser, winner)
      canonicalPair = makeCanonicalPair opt1 opt2

  reversePrefExisted <- MonadState.gets (Set.member reversePref . Map.findWithDefault Set.empty userId . statePreferences)

  Monad.when reversePrefExisted $
    MonadState.liftIO $ putStrLn $
      "** Preference Reversal: " ++ show loser ++ " > " ++ show winner ++ " with " ++ show winner ++ " > " ++ show loser

  currentUserPrefs <- MonadState.gets (Map.findWithDefault Set.empty userId . statePreferences)
  currentUserUncompared <- MonadState.gets (Map.findWithDefault Set.empty userId . stateUncomparedPairs)

  let userPrefsWithoutReverse = Set.delete reversePref currentUserPrefs
      newUserPrefs = Set.insert newPref userPrefsWithoutReverse
      newUserUncompared = Set.delete canonicalPair currentUserUncompared

  MonadState.modify $ \s ->
    s { statePreferences = Map.insert userId newUserPrefs (statePreferences s)
      , stateUncomparedPairs = Map.insert userId newUserUncompared (stateUncomparedPairs s)
      , stateNextTimestamp = timestamp + 1
      }

  updateViolationsOnNewPreference userId newPref (if reversePrefExisted then Just reversePref else Nothing)

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

updateViolationsOnNewPreference :: UserId -> (Option, Option) -> Maybe (Option, Option) -> App ()
updateViolationsOnNewPreference userId (winner, loser) mRemovedPref = do
  prefsMap <- MonadState.gets statePreferences
  optionsSet <- MonadState.gets stateOptions
  let userPrefs = Map.findWithDefault Set.empty userId prefsMap

  currentViolations <- MonadState.gets (Map.findWithDefault Set.empty userId . stateViolations)

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

  let finalViolations = violationsAfterRemoval `Set.union` newViolations
      addedCount = Set.size newViolations

  MonadState.modify $ \s -> s { stateViolations = Map.insert userId finalViolations (stateViolations s) }

  Monad.unless (addedCount == 0 && removedCount == 0) $
    MonadState.liftIO $ Printf.printf "Violations update for %s: +%d, -%d. Total: %d\n" (show userId) addedCount removedCount (Set.size finalViolations)

calculateTransitivityScore :: UserId -> App Double
calculateTransitivityScore userId = do
  options <- MonadState.gets (Set.toList . stateOptions)
  violationsSet <- getViolations userId

  pure $ transitivityScore (length options) (Set.size violationsSet)
  where
    transitivityScore :: Int -> Int -> Double
    transitivityScore n violationsCount
      | violationsCount == 0 = 1.0
      | otherwise = 1.0 - (fromIntegral violationsCount / totalPossibleTriples n)

    totalPossibleTriples :: Int -> Double
    totalPossibleTriples n
      | n < 3 = 1.0
      | otherwise = fromIntegral $ div (n * (n - 1) * (n - 2)) 6

calculateAgreementScore :: UserId -> App Double
calculateAgreementScore userId = do
  eloRatingsList <- getUserRatings userId
  let eloRatingsMap = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) eloRatingsList
  preferences <- MonadState.gets statePreferences
  let userPrefs = Map.findWithDefault Set.empty userId preferences
      comparedPairs = Set.toList userPrefs
      totalCompared = length comparedPairs

  if totalCompared == 0
    then pure 1.0
    else do
      initialRating <- MonadReader.asks configInitialRating
      let (concordant, discordant) = foldl (countAgreement eloRatingsMap initialRating) (0, 0) comparedPairs
      let tau = (fromIntegral concordant - fromIntegral discordant) / fromIntegral totalCompared
      pure $ (tau + 1.0) / 2.0
  where
    countAgreement :: Map.Map OptionId Double -> Double -> (Int, Int) -> (Option, Option) -> (Int, Int)
    countAgreement eloMap defaultRating (conc, disc) (preferredOption, nonPreferredOption) =
      let prefId = optionId preferredOption
          nonPrefId = optionId nonPreferredOption
          lookupRating oid = Map.findWithDefault defaultRating oid eloMap
      in
      let ratingPref = lookupRating prefId
          ratingNonPref = lookupRating nonPrefId
      in
      if ratingPref > ratingNonPref
        then (conc + 1, disc)
      else if ratingPref < ratingNonPref
        then (conc, disc + 1)
      else
        (conc, disc)

presentComparison :: UserId -> Option -> Option -> App MatchResult
presentComparison userId option1 option2 = do
  swapOrder <- MonadState.liftIO Random.randomIO :: App Bool

  MonadState.liftIO $ do
    putStrLn $ "User: " ++ show userId
    putStrLn "Choose between:"
    putStrLn $ "1. " ++ show (optionName $ if swapOrder then option2 else option1)
    putStrLn $ "2. " ++ show (optionName $ if swapOrder then option1 else option2)

  choice <- MonadState.liftIO getLine

  pure $ case (choice, swapOrder) of
    ("1", False) -> Win
    ("2", False) -> Loss
    ("1", True)  -> Loss
    ("2", True)  -> Win
    (_,   _)     -> Loss

runEvaluationSession :: UserId -> App ()
runEvaluationSession userId = continueSession (1 :: Int)
  where
    continueSession comparisonNum = do
      maybePair <- getNextComparisonPair userId

      case maybePair of
        Nothing -> do
          isTrulyComplete <- checkIfComplete userId
          if isTrulyComplete
            then MonadState.liftIO $ putStrLn "\nComplete and consistent ranking achieved!"
            else MonadState.liftIO $ putStrLn "\nNo more comparison pairs available (check logic if unexpected)."

        Just (option1, option2) -> do
          prevRankings <- getPreviousRankings userId
          prevRatingsMap <- getPreviousRatings userId

          MonadState.liftIO $ putStrLn $ "\nComparison " ++ show comparisonNum ++ ":"
          result <- presentComparison userId option1 option2

          recordComparison userId option1 option2 result
          updateRatings userId option1 option2 result

          displayRankings userId prevRankings prevRatingsMap
          displaySessionStatus userId

          continueSession (comparisonNum + 1)

getPreviousRatings :: UserId -> App (Map.Map OptionId Double)
getPreviousRatings userId = do
  ratings <- getUserRatings userId
  pure $ makeRatingsMap ratings
  where
    makeRatingsMap :: [(Option, Double)] -> Map.Map OptionId Double
    makeRatingsMap ratings = Map.fromList $
      map (\(opt, rating) -> (optionId opt, rating)) ratings

getPreviousRankings :: UserId -> App [(Option, Int)]
getPreviousRankings userId = do
  ratings <- getUserRatings userId
  pure $ makeRanking ratings
  where
    makeRanking :: [(Option, Double)] -> [(Option, Int)]
    makeRanking ratings = zip (map fst ratings) [1..]

displayRankings :: UserId -> [(Option, Int)] -> Map.Map OptionId Double -> App ()
displayRankings userId prevRankings prevRatingsMap = do
  currentRatingsList <- getUserRatings userId
  initialRating <- MonadReader.asks configInitialRating

  let currentRatingsMap = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) currentRatingsList
      prevRankMap = Map.fromList $ map (\(opt, rank) -> (optionId opt, rank)) prevRankings
      currentWithRanks = zip (map fst currentRatingsList) [1..]

      formatChange :: (Option, Int) -> String
      formatChange (option, currentRank) = do
        let oid = optionId option
            prevRank = Map.findWithDefault currentRank oid prevRankMap

            currentRating = Map.findWithDefault initialRating oid currentRatingsMap

            prevRating = Map.findWithDefault initialRating oid prevRatingsMap

            rankChange = prevRank - currentRank
            ratingChange = currentRating - prevRating

            rankChangeStr = case compare rankChange 0 of
              EQ -> ""
              GT -> " (↑" ++ show rankChange ++ ")"
              LT -> " (↓" ++ show (abs rankChange) ++ ")"

            ratingChangeStr = case compare ratingChange 0 of
              EQ -> ""
              GT -> " (+" ++ Printf.printf "%.2f" ratingChange ++ ")"
              LT -> " (-" ++ Printf.printf "%.2f" (abs ratingChange) ++ ")"

        "  " ++ show currentRank ++ ". " ++ show (optionName option) ++ ": " ++ Printf.printf "%.2f" currentRating ++ ratingChangeStr ++ rankChangeStr

  MonadState.liftIO $ do
    putStrLn $ "Updated ratings for user " ++ show userId ++ ":"
    mapM_ (putStrLn . formatChange) currentWithRanks

displaySessionStatus :: UserId -> App ()
displaySessionStatus userId = do
  optionsCount <- MonadState.gets (Set.size . stateOptions)
  uncomparedSet <- MonadState.gets (Map.findWithDefault Set.empty userId . stateUncomparedPairs)
  violationsSet <- getViolations userId

  let totalPossiblePairs = if optionsCount < 2 then 0 else div (optionsCount * (optionsCount - 1)) 2
      completedPairs = totalPossiblePairs - Set.size uncomparedSet
      violationsCount = Set.size violationsSet

  agreementScore <- calculateAgreementScore userId
  transitivityScore <- calculateTransitivityScore userId

  MonadState.liftIO $ do
    Printf.printf "Progress: %d/%d pairs compared.\n" completedPairs totalPossiblePairs
    Printf.printf "Stability: %.2f%%\n" (agreementScore * 100)
    Printf.printf "Consistency: %.2f%%\n" (transitivityScore * 100)

    Monad.unless (Set.null violationsSet) $ do
        Printf.printf "Detected %d transitivity violations.\n" violationsCount
        putStrLn "Specific violations (up to 5 shown):"
        let displayLimit = 5
            violationsToDisplay = take displayLimit (Set.toList violationsSet)
            formattedViolations = map formatViolation violationsToDisplay
        mapM_ (\(i, v) -> Printf.printf "  %d. %s\n" i v) (zip [1 :: Int ..] formattedViolations)
        Monad.when (violationsCount > displayLimit) $
          Printf.printf "  (and %d more violations not shown)\n" (violationsCount - displayLimit)

    Monad.when (Set.null uncomparedSet && Set.null violationsSet) $ do
      putStrLn "All pairs compared and preference set is internally consistent."
  where
    formatViolation :: (Option, Option, Option) -> String
    formatViolation (a, c, b) =
      Printf.printf "%s > %s, %s > %s, but %s > %s (inconsistent)"
        (show $ optionName a) (show $ optionName b)
        (show $ optionName b) (show $ optionName c)
        (show $ optionName a) (show $ optionName c)

main :: IO ()
main = MonadState.evalStateT (MonadReader.runReaderT app defaultConfig) initialState
  where
    app = do
      let options =
            [ createOption "red" "Red"
            , createOption "orange" "Orange"
            , createOption "yellow" "Yellow"
            , createOption "green" "Green"
            , createOption "blue" "Blue"
            , createOption "violet" "Violet"
            ]
      Monad.forM_ options addOption

      let user = createUser "coriocactus"
      addUser user

      runEvaluationSession user

      MonadState.liftIO $ putStrLn "\n--- Final Results ---"
      finalRankings <- getUserRatings user
      MonadState.liftIO $ putStrLn $ "Final ratings for user " ++ show user ++ ":"
      Monad.forM_ finalRankings $ \(option, rating) ->
        MonadState.liftIO $ putStrLn $ "  " ++ show (optionName option) ++ ": " ++ Printf.printf "%.2f" rating

      displaySessionStatus user

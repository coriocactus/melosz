{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
    options <- MonadState.gets stateOptions

    MonadState.modify $ \s -> s
      { stateUsers = Set.insert userId (stateUsers s)
      , statePreferences = Map.insert userId Set.empty (statePreferences s)
      , stateViolations = Map.insert userId Set.empty (stateViolations s)
      , stateUncomparedPairs = Map.insert userId (getAllOptionPairsSet options) (stateUncomparedPairs s)
      }
    where
      getAllOptionPairsSet :: Set.Set Option -> Set.Set (Option, Option)
      getAllOptionPairsSet options = Set.fromList $ getAllOptionPairs (Set.toList options)

getRating :: UserId -> Option -> App Double
getRating uid option = do
  ratings <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating
  pure $ Map.findWithDefault initialRating (uid, optionId option) ratings

getUserRatings :: UserId -> App [(Option, Double)]
getUserRatings userId = do
  options <- MonadState.gets stateOptions
  ratings <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating

  pure $ sortByRating $ findUserRatings options initialRating ratings
  where
    sortByRating :: [(Option, Double)] -> [(Option, Double)]
    sortByRating = List.sortBy (\(_, r1) (_, r2) -> compare r2 r1)

    findUserRatings :: Set.Set Option -> Double -> Map.Map (UserId, OptionId) Double -> [(Option, Double)]
    findUserRatings options initialRating ratings = map (\option -> findUserRating option initialRating ratings) (Set.toList options)

    findUserRating :: Option -> Double -> Map.Map (UserId, OptionId) Double -> (Option, Double)
    findUserRating option initialRating ratings = (option, Map.findWithDefault initialRating (userId, optionId option) ratings)

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
  violationPairs <- findMinimalTransitivityViolations userId
  uncomparedSet <- MonadState.gets (Map.findWithDefault Set.empty userId
                                   . stateUncomparedPairs)
  isDone <- checkIfComplete userId

  Monad.msum
    [ Monad.guard (not (null violationPairs)) >> selectMixedPairs violationPairs userId
    , Monad.guard (not (Set.null uncomparedSet)) >> selectRandomPair (uncomparedList uncomparedSet) (length (uncomparedList uncomparedSet) - 1)
    , Monad.guard (not isDone) >> getRandomPairForRefinement userId
    , pure Nothing
    ]
  where
    uncomparedList :: Set.Set (Option, Option) -> [(Option, Option)]
    uncomparedList = Set.toList

    selectRandomPair :: [(Option, Option)] -> Int -> App (Maybe (Option, Option))
    selectRandomPair pairs maxIdx = do
      idx <- MonadState.liftIO $ Random.randomRIO (0, min maxIdx (length pairs - 1))
      pure $ Just (pairs !! idx)

    selectMixedPairs :: [(Option, Option)] -> UserId -> App (Maybe (Option, Option))
    selectMixedPairs violationPairs uid = do
      prefs <- MonadState.gets (Map.findWithDefault Set.empty uid . statePreferences)

      let violationCount = length violationPairs
          prefPairs = Set.toList prefs

      Monad.msum
        [ Monad.guard (violationCount > 5) >> selectRandomPair violationPairs 4
        , Monad.guard (null prefPairs) >> selectRandomPair violationPairs (min 4 (violationCount - 1))
        , do
            useViolation <- MonadState.liftIO $ Random.randomRIO (0 :: Int, 2)
            Monad.msum
              [ Monad.guard (useViolation > 0 || null violationPairs) >> selectFromPairs prefPairs
              , selectRandomPair violationPairs (violationCount - 1)
              ]
        ]

    selectFromPairs :: [(Option, Option)] -> App (Maybe (Option, Option))
    selectFromPairs pairs = do
      idx <- MonadState.liftIO $ Random.randomRIO (0, length pairs - 1)
      pure $ Just (pairs !! idx)


getViolations :: UserId -> App (Set.Set (Option, Option, Option))
getViolations userId =
  MonadState.gets (Map.findWithDefault Set.empty userId . stateViolations)

getTransitivityViolationPair :: UserId -> App (Maybe (Option, Option))
getTransitivityViolationPair userId = do
  violations <- getViolations userId
  if Set.null violations
    then getRandomPairForRefinement userId
    else do
      let violationList = Set.toList violations
          involvedPairs = concatMap (\(a, b, c) -> [makeCanonicalPair a b, makeCanonicalPair b c, makeCanonicalPair a c]) violationList
      idx <- MonadState.liftIO $ Random.randomRIO (0, length involvedPairs - 1)
      pure $ Just (involvedPairs !! idx)

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

getRandomPairForRefinement :: UserId -> App (Maybe (Option, Option))
getRandomPairForRefinement _ = do
  options <- MonadState.gets (Set.toList . stateOptions)
  if length options < 2
    then pure Nothing
    else do
      i <- MonadState.liftIO $ Random.randomRIO (0, length options - 1)
      let option1 = options !! i
      j <- MonadState.liftIO $ Random.randomRIO (0, length options - 2)
      let option2 = options !! (if j >= i then j + 1 else j)
      pure $ Just (option1, option2)

recordComparison :: UserId -> Option -> Option -> MatchResult -> App ()
recordComparison userId opt1 opt2 result = do
  timestamp <- MonadState.gets stateNextTimestamp

  let comparison = Comparison userId opt1 opt2 result timestamp
  MonadState.liftIO $ putStrLn $ "Logged:" <> show comparison

  let (winner, loser) = case result of
        Win -> (opt1, opt2)
        Loss -> (opt2, opt1)
      newPref = (winner, loser)
      canonicalPair = makeCanonicalPair opt1 opt2

  prefsMap <- MonadState.gets statePreferences
  let existingUserPrefs = Map.findWithDefault Set.empty userId prefsMap
  Monad.when (isPreferred loser winner existingUserPrefs) $
    MonadState.liftIO $ putStrLn $ "** Warning: Overwriting existing preference " ++ show loser ++ " > " ++ show winner ++ " with " ++ show winner ++ " > " ++ show loser

  MonadState.modify $ \s ->
    let prefs = statePreferences s
        userPrefs = Map.findWithDefault Set.empty userId prefs
        newUserPrefs = Set.insert newPref userPrefs

        uncompared = stateUncomparedPairs s
        userUncompared = Map.findWithDefault Set.empty userId uncompared
        newUserUncompared = Set.delete canonicalPair userUncompared

    in s { statePreferences = Map.insert userId newUserPrefs prefs
         , stateUncomparedPairs = Map.insert userId newUserUncompared uncompared
         , stateNextTimestamp = timestamp + 1
         }

  updateViolationsIncremental userId winner loser

updateViolationsIncremental :: UserId -> Option -> Option -> App ()
updateViolationsIncremental userId winner loser = do
  prefsMap <- MonadState.gets statePreferences
  optionsSet <- MonadState.gets stateOptions
  let userPrefs = Map.findWithDefault Set.empty userId prefsMap
      options = Set.toList optionsSet

  let newlyCreatedViolations = Set.fromList $ do
        x <- options
        Monad.guard (optionId x /= optionId winner && optionId x /= optionId loser)
        Monad.guard (isPreferred loser x userPrefs && isPreferred x winner userPrefs)
        pure (winner, loser, x)

  MonadState.modify $ \s ->
    let violationsMap = stateViolations s
        userViolations = Map.findWithDefault Set.empty userId violationsMap
        updatedViolations = Set.union userViolations newlyCreatedViolations
    in s { stateViolations = Map.insert userId updatedViolations violationsMap }

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
  currentRatings <- getUserRatings userId
  initialRating <- MonadReader.asks configInitialRating

  let prevRankMap = Map.fromList $
        map (\(opt, rank) -> (optionId opt, rank)) prevRankings

      currentWithRanks = zip (map fst currentRatings) [1..]

      formatChange :: (Option, Int) -> String
      formatChange (option, currentRank) = do
        let oid = optionId option
            prevRank = Map.findWithDefault currentRank oid prevRankMap

            currentRating = case filter (\(opt, _) -> optionId opt == oid)
                                  currentRatings of
                (_, rating):_ -> rating
                [] -> initialRating

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
  minimalViolations <- findMinimalTransitivityViolations userId

  let totalPossiblePairs = div (optionsCount * (optionsCount - 1)) 2
      completedPairs = totalPossiblePairs - Set.size uncomparedSet
      violationsCount = Set.size violationsSet

  agreementScore <- calculateAgreementScore userId
  transitivityScore <- calculateTransitivityScore userId

  MonadState.liftIO $ do
    putStrLn $ "Progress: " ++ show completedPairs ++ "/" ++ show totalPossiblePairs ++ " pairs compared."
    putStrLn $ "Stability: " ++ Printf.printf "%.2f" (agreementScore * 100) ++ "%"
    putStrLn $ "Consistency: " ++ Printf.printf "%.2f" (transitivityScore * 100) ++ "%"

    Monad.when (violationsCount > 0) $ do
      putStrLn $ "Detected " ++ show violationsCount ++ " transitivity violations."
      let topViolators = take 5 minimalViolations
      Monad.unless (null topViolators) $
        putStrLn $ "  Pairs frequently involved in violations: " ++ show (map (\(o1,o2) -> (optionName o1, optionName o2)) topViolators)

    Monad.when (Set.null uncomparedSet && Set.null violationsSet) $ do
      putStrLn "All pairs compared and preference set is internally consistent."

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

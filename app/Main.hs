{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import qualified System.Random as Random
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

newtype UserId = UserId B.ByteString
  deriving (Show, Eq, Ord)

data Option = Option
  { optionId :: B.ByteString
  , optionName :: B.ByteString
  } deriving (Show, Eq, Ord)

data MatchResult = Win | Draw | Loss
  deriving (Show, Eq)

data AppState = AppState
  { stateUsers :: Set.Set UserId
  , stateOptions :: Set.Set Option
  , stateRatings :: Map.Map (UserId, B.ByteString) Double
  , stateComparisons :: [Comparison]
  , stateNextTimestamp :: Int
  }

data AppConfig = AppConfig
  { configKFactor :: Double
  , configInitialRating :: Double
  }

type App = MonadReader.ReaderT AppConfig (MonadState.StateT AppState IO)

createOption :: B.ByteString -> B.ByteString -> Option
createOption oid name = Option oid name

createUser :: B.ByteString -> UserId
createUser = UserId

getRating :: UserId -> Option -> App Double
getRating userId option = do
  ratings <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating
  pure $ Map.findWithDefault initialRating
    (userId, optionId option) ratings

calculateExpectedScore :: Double -> Double -> Double
calculateExpectedScore aRating bRating =
  1.0 / (1.0 + 10.0 ** ((bRating - aRating) / 400.0))

resultToScore :: MatchResult -> Double
resultToScore Win  = 1.0
resultToScore Draw = 0.5
resultToScore Loss = 0.0

flipResult :: MatchResult -> MatchResult
flipResult Win  = Loss
flipResult Loss = Win
flipResult Draw = Draw

updateRating :: UserId -> Option -> Option -> MatchResult -> App ()
updateRating userId option opponent result = do
  kFactor <- MonadReader.asks configKFactor
  optionRating <- getRating userId option
  opponentRating <- getRating userId opponent

  let expected = calculateExpectedScore optionRating opponentRating
      actual = resultToScore result
      newRating = optionRating + kFactor * (actual - expected)

  MonadState.modify $ \s -> s { stateRatings = Map.insert
    (userId, optionId option) newRating (stateRatings s) }

updateRatings :: UserId -> Option -> Option -> MatchResult -> App ()
updateRatings userId option1 option2 result = do
  updateRating userId option1 option2 result
  updateRating userId option2 option1 (flipResult result)

getRandomOptionPair :: App (Maybe (Option, Option))
getRandomOptionPair = do
  options <- MonadState.gets (Set.toList . stateOptions)
  if length options < 2
    then pure Nothing
    else do
      i <- MonadState.liftIO $ Random.randomRIO (0, length options - 1)
      let option1 = options !! i
      j <- MonadState.liftIO $ Random.randomRIO (0, length options - 2)
      let option2 = options !! (if j >= i then j + 1 else j)
      pure $ Just (option1, option2)

presentComparison :: UserId -> Option -> Option -> App MatchResult
presentComparison userId option1 option2 = do
  MonadState.liftIO $ putStrLn $ "User: " ++ show userId
  MonadState.liftIO $ putStrLn "Choose between:"
  MonadState.liftIO $ putStrLn $ "1. " ++ show (optionName option1)
  MonadState.liftIO $ putStrLn $ "2. " ++ show (optionName option2)

  choice <- MonadState.liftIO getLine
  pure $ case choice of
    "1" -> Win
    _   -> Loss

addOption :: Option -> App ()
addOption option = MonadState.modify $ \s ->
  s { stateOptions = Set.insert option (stateOptions s) }

addUser :: UserId -> App ()
addUser userId = MonadState.modify $ \s ->
  s { stateUsers = Set.insert userId (stateUsers s) }

getUserRatings :: UserId -> App [(Option, Double)]
getUserRatings userId = do
  options <- MonadState.gets stateOptions
  ratings <- MonadState.gets stateRatings
  initialRating <- MonadReader.asks configInitialRating

  let userRatings = flip map (Set.toList options) $ \option ->
        let rating = Map.findWithDefault initialRating (userId, optionId option) ratings
        in (option, rating)

  pure $ sortByRating userRatings
  where
    sortByRating = List.sortBy (\(_, r1) (_, r2) -> compare r2 r1)

runEvaluationSession :: UserId -> Int -> App ()
runEvaluationSession userId numComparisons = do
  Monad.forM_ [1..numComparisons] $ \i -> do
    optionPair <- getNextComparisonPair userId
    case optionPair of
      Nothing -> MonadState.liftIO $ putStrLn "Not enough options to compare"
      Just (option1, option2) -> do
        MonadState.liftIO $ putStrLn $ "Comparison " ++ show i ++ ":"
        result <- presentComparison userId option1 option2

        recordComparison userId option1 option2 result

        updateRatings userId option1 option2 result

        displaySessionStatus userId
        displayRatings userId
        MonadState.liftIO $ putStrLn ""

initialState :: AppState
initialState = AppState
  { stateUsers = Set.empty
  , stateOptions = Set.empty
  , stateRatings = Map.empty
  , stateComparisons = []
  , stateNextTimestamp = 0
  }

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configKFactor = 32.0
  , configInitialRating = 1500.0
  }

displayRatings :: UserId -> App ()
displayRatings userId = do
  ratings <- getUserRatings userId
  MonadState.liftIO $ putStrLn $ "Ratings for user " ++ show userId ++ ":"
  Monad.forM_ ratings $ \(option, rating) ->
    MonadState.liftIO $ putStrLn $ "  " ++ show (optionName option) ++ ": " ++ show rating

data Comparison = Comparison
  { compUser :: UserId
  , compOption1 :: Option
  , compOption2 :: Option
  , compResult :: MatchResult
  , compTimestamp :: Int
  } deriving (Show, Eq)


getAllOptionPairs :: [Option] -> [(Option, Option)]
getAllOptionPairs options = do
  (i, opt1) <- zip [0 :: Int ..] options
  (j, opt2) <- zip [0 :: Int ..] options
  Monad.guard (i < j)
  pure (opt1, opt2)

hasBeenCompared :: UserId -> Option -> Option -> [Comparison] -> Bool
hasBeenCompared userId opt1 opt2 comparisons =
  any (\comp -> compUser comp == userId &&
               ((compOption1 comp == opt1 && compOption2 comp == opt2) ||
                (compOption1 comp == opt2 && compOption2 comp == opt1)))
      comparisons

getUncomparedPairs :: UserId -> [Option] -> [Comparison] -> [(Option, Option)]
getUncomparedPairs userId options comparisons =
  filter (\(opt1, opt2) -> not $ hasBeenCompared userId opt1 opt2 comparisons)
         (getAllOptionPairs options)

getNextComparisonPair :: UserId -> App (Maybe (Option, Option))
getNextComparisonPair userId = do
  options <- MonadState.gets (Set.toList . stateOptions)
  comparisons <- MonadState.gets stateComparisons

  let uncomparedPairs = getUncomparedPairs userId options comparisons

  if null uncomparedPairs
    then getTransitivityViolationPair userId
    else do
      idx <- MonadState.liftIO $ Random.randomRIO (0, length uncomparedPairs - 1)
      pure $ Just (uncomparedPairs !! idx)

-- check if a > b and b > c implies a > c
-- returns pairs that violate transitivity
findTransitivityViolations :: UserId -> [Comparison] -> [(Option, Option)]
findTransitivityViolations userId comparisons = do
  let userComps = filter (\c -> compUser c == userId) comparisons

      -- create a map from (opt1, opt2) to result
      resultMap = Map.fromList $
        map (\c -> ((compOption1 c, compOption2 c), compResult c)) userComps

      -- get all options that have been compared
      comparedOptions = Set.toList $ Set.fromList $
        concatMap (\c -> [compOption1 c, compOption2 c]) userComps

      -- get result for a pair, handling both orientations
      getResult opt1 opt2 =
        case Map.lookup (opt1, opt2) resultMap of
          Just res -> Just res
          Nothing -> case Map.lookup (opt2, opt1) resultMap of
                      Just res -> Just (flipResult res)
                      Nothing -> Nothing

  -- find all triples where transitivity is violated
  let violations = do
        a <- comparedOptions
        b <- comparedOptions
        c <- comparedOptions
        Monad.guard (a /= b && b /= c && a /= c)

        case (getResult a b, getResult b c, getResult a c) of
          (Just Win, Just Win, Just Loss) -> [(a, c)]   -- a>b, b>c, but a<c
          (Just Loss, Just Loss, Just Win) -> [(c, a)]  -- a<b, b<c, but a>c
          _ -> []

  Set.toList $ Set.fromList violations

-- get a pair that violates transitivity, or a random pair if none
getTransitivityViolationPair :: UserId -> App (Maybe (Option, Option))
getTransitivityViolationPair userId = do
  comparisons <- MonadState.gets stateComparisons
  let violations = findTransitivityViolations userId comparisons

  if null violations
    then getRandomPairForRefinement userId
    else do
      idx <- MonadState.liftIO $ Random.randomRIO (0, length violations - 1)
      pure $ Just (violations !! idx)

-- when we have no violations and all pairs compared, get random pair for refinement
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
  let comparison = Comparison
        { compUser = userId
        , compOption1 = opt1
        , compOption2 = opt2
        , compResult = result
        , compTimestamp = timestamp
        }

  MonadState.modify $ \s -> s
    { stateComparisons = comparison : stateComparisons s
    , stateNextTimestamp = timestamp + 1
    }

-- calculate ranking stability (0.0-1.0)
-- higher means more stable (fewer transitivity violations)
calculateStability :: UserId -> App Double
calculateStability userId = do
  comparisons <- MonadState.gets stateComparisons
  options <- MonadState.gets (Set.toList . stateOptions)

  let violations = findTransitivityViolations userId comparisons

      totalPossible =
        if length options < 3
        then 1  -- avoid division by zero
        else length options * (length options - 1) * (length options - 2) `div` 6

  pure $ 1.0 - (fromIntegral (length violations) / fromIntegral totalPossible)

displaySessionStatus :: UserId -> App ()
displaySessionStatus userId = do
  options <- MonadState.gets (Set.toList . stateOptions)
  comparisons <- MonadState.gets stateComparisons

  let userComps = filter (\c -> compUser c == userId) comparisons
      totalPossiblePairs = length options * (length options - 1) `div` 2
      completedPairs = length $ Set.fromList $ map
        (\c -> (min (compOption1 c) (compOption2 c),
                max (compOption1 c) (compOption2 c))) userComps

  stability <- calculateStability userId

  MonadState.liftIO $ do
    putStrLn $ "Completion: " ++ show completedPairs ++ "/"
      ++ show totalPossiblePairs ++ " pairs compared"
    putStrLn $ "Stability: " ++ show (stability * 100) ++ "%"

    -- if we've compared all pairs, show if we have transitivity violations
    Monad.when (completedPairs == totalPossiblePairs) $ do
      let violations = findTransitivityViolations userId comparisons
      if null violations
        then putStrLn "Complete and consistent ranking achieved!"
        else putStrLn $ "Transitivity violations detected: " ++ show (length violations)

main :: IO ()
main = MonadState.evalStateT (MonadReader.runReaderT app defaultConfig)
  initialState
  where
    app = do
      let options =
            [ createOption "red" "red"
            , createOption "orange" "orange"
            , createOption "yellow" "yellow"
            , createOption "green" "green"
            , createOption "lime" "lime"
            , createOption "blue" "blue"
            , createOption "violet" "violet"
            ]
      Monad.forM_ options addOption

      let user = createUser "user"
          -- we'll do more comparisons than strictly required to allow for transitivity checks
          totalComparisons = length options * (length options - 1)

      addUser user
      runEvaluationSession user totalComparisons

      MonadState.liftIO $ putStrLn "Final rankings:"
      displayRatings user
      displaySessionStatus user

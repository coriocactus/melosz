{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random as Random
import qualified Text.Printf as Printf

import Types
import AppState
import Rating
import Marshal
import Scheduler
import Preference

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Console Runner

main :: IO ()
main = runConsole

runConsole :: IO ()
runConsole = do
  let appToRun :: App ()
      appToRun = colourfulScaffold >>=
        (\(options, user) -> runInteractiveSession user options)
  runner appToRun

runner :: App a -> IO a
runner app = do
  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let config = AppConfig
        { configKFactor = 32.0
        , configInitialRating = 1500.0
        , configStateRef = initialStateRef
        }
  MonadReader.runReaderT app config

colourfulScaffold :: App ([Option], UserId)
colourfulScaffold = do
  let options =
        [ createOption "red" "Red" ""
        , createOption "orange" "Orange" ""
        , createOption "yellow" "Yellow" ""
        , createOption "green" "Green" ""
        , createOption "blue" "Blue" ""
        , createOption "violet" "Violet" ""
        , createOption "indigo" "Indigo" ""
        , createOption "cyan" "Cyan" ""
        , createOption "magenta" "Magenta" ""
        ]
  let user = "coriocactus"

  setupOptions options
  setupUser user

  pure (options, user)

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|

runInteractiveSession :: UserId -> [Option] -> App ()
runInteractiveSession user options = do
  MonadIO.liftIO $ putStrLn "Starting ConsoleUI..."
  runEvaluationSession user
  reportFinalStatus user options

runEvaluationSession :: UserId -> App ()
runEvaluationSession uid = continueSession (1 :: Int)
  where
    continueSession comparisonNum = do
      options <- getOptions
      ratings <- getUserRatings uid $ Set.toList options
      violations <- getViolationsForUser uid

      let violationPairs = findPairsInViolations violations

      maybePair <- getNextComparisonPair uid ratings violationPairs

      case maybePair of
        Nothing -> do
          isTrulyComplete <- checkIfComplete uid
          MonadIO.liftIO $ putStrLn ""
          if isTrulyComplete
            then MonadIO.liftIO $ putStrLn ">>> Ranking appears complete and consistent. <<<"
            else MonadIO.liftIO $ putStrLn ">>> No more comparison pairs available based on current strategy (may indicate completion or need for different pair selection logic). <<<"

        Just (option1, option2) -> do
          let prevRankings = ratingsToRankings ratings
          let prevRatingsMap = ratingsToMap ratings

          MonadIO.liftIO $ Printf.printf "\n--- Comparison %d ---\n" comparisonNum
          result <- presentComparison uid option1 option2

          recordComparison uid option1 option2 result
          updateRatings uid option1 option2 result

          displayRankings uid prevRankings prevRatingsMap
          displaySessionStatus uid

          continueSession (comparisonNum + 1)

reportFinalStatus :: UserId -> [Option] -> App ()
reportFinalStatus user options = do
  MonadIO.liftIO $ putStrLn "\n--- Final Results ---"
  finalRankingsWithScores <- getUserRatings user options

  MonadIO.liftIO $ putStrLn $ "Final ratings for user " ++ show user ++ ":"
  Monad.forM_ (zip [1 :: Int ..] finalRankingsWithScores) $ \(rank, (option, rating)) ->
      MonadIO.liftIO $ Printf.printf "  %d. %s: %.2f\n" rank (show $ optionName option) rating

  displaySessionStatus user

  MonadIO.liftIO $ putStrLn "\nEvaluation finished."

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|

presentComparison :: UserId -> Option -> Option -> App MatchResult
presentComparison uid option1 option2 = do
  swapOrder <- MonadIO.liftIO Random.randomIO :: App Bool
  let (dispOpt1, dispOpt2) =
        if swapOrder then (option2, option1) else (option1, option2)

  MonadIO.liftIO $ do
    putStrLn $ "User: " ++ show uid
    putStrLn "Choose between:"
    Printf.printf "1. %s\n" (show $ optionName dispOpt1)
    Printf.printf "2. %s\n" (show $ optionName dispOpt2)
    putStr "Your choice (1 or 2): "

  getValidChoice swapOrder dispOpt1 dispOpt2
  where
    getValidChoice :: Bool -> Option -> Option -> App MatchResult
    getValidChoice swapOrder dispOpt1 dispOpt2 = do
      choice <- MonadIO.liftIO getLine
      case choice of
        "1" -> pure $ if swapOrder then Loss else Win
        "2" -> pure $ if swapOrder then Win else Loss
        _   -> do
          MonadIO.liftIO $ do
            putStrLn "Invalid choice. Please enter 1 or 2."
            Printf.printf "1. %s\n" (show $ optionName dispOpt1)
            Printf.printf "2. %s\n" (show $ optionName dispOpt2)
            putStr "Your choice (1 or 2): "
          getValidChoice swapOrder dispOpt1 dispOpt2

formatViolation :: (Option, Option, Option) -> String
formatViolation (a, c, b) =
  Printf.printf "%s > %s, %s > %s, %s > %s"
    (show $ optionName a) (show $ optionName b)
    (show $ optionName b) (show $ optionName c)
    (show $ optionName c) (show $ optionName a)

displayRankings :: UserId -> [(Option, Int)] -> Map.Map OptionId Double -> App ()
displayRankings uid prevRankings prevRatingsMap = do
  options <- getOptions
  currentRatingsList <- getUserRatings uid $ Set.toList options
  config <- getConfig
  let initialRating = configInitialRating config

  let currentRatingsMap = Map.fromList $ map (\(opt, rating) -> (optionId opt, rating)) currentRatingsList
      prevRankMap = Map.fromList $ map (\(opt, rank) -> (optionId opt, rank)) prevRankings
      currentWithRanks = zip (map fst currentRatingsList) [1..]

      formatChange :: (Option, Int) -> String
      formatChange (option, currentRank) =
        let oid = optionId option
            prevRank = Map.findWithDefault currentRank oid prevRankMap
            currentRating = Map.findWithDefault initialRating oid currentRatingsMap
            prevRating = Map.findWithDefault initialRating oid prevRatingsMap

            rankChange = prevRank - currentRank
            ratingChange = currentRating - prevRating

            rankChangeStr = case compare rankChange 0 of
              EQ -> "" :: String
              GT -> Printf.printf " (↑%d)" rankChange
              LT -> Printf.printf " (↓%d)" (abs rankChange)

            ratingChangeStr = case compare ratingChange 0 of
              EQ -> "" :: String
              GT -> Printf.printf " (+%.2f)" ratingChange
              LT -> Printf.printf " (-%.2f)" (abs ratingChange)

        in Printf.printf "  %d. %s: %.2f%s%s" currentRank (show $ optionName option) currentRating ratingChangeStr rankChangeStr

  MonadIO.liftIO $ do
    putStrLn $ "\nUpdated ratings for user " ++ show uid ++ ":"
    mapM_ (putStrLn . formatChange) currentWithRanks

displaySessionStatus :: UserId -> App ()
displaySessionStatus uid = do
  options <- getOptions
  ratings <- getUserRatings uid $ Set.toList options
  optionsCount <- Set.size <$> getOptions
  uncomparedSet <- getUncomparedPairsForUser uid
  violationsSet <- getViolationsForUser uid

  let totalPossiblePairs = if optionsCount < 2 then 0 else optionsCount * (optionsCount - 1) `div` 2
      completedPairs = totalPossiblePairs - Set.size uncomparedSet
      violationsCount = Set.size violationsSet

  agreementScore <- calculateAgreementScore uid ratings
  transitivityScore <- calculateTransitivityScore uid

  MonadIO.liftIO $ do
    putStrLn ""
    Printf.printf "Progress: %d/%d pairs compared (%.1f%%).\n" completedPairs totalPossiblePairs (if totalPossiblePairs == 0 then 100.0 else (fromIntegral completedPairs * 100.0 / fromIntegral totalPossiblePairs :: Double))
    Printf.printf "Agreement (vs ELO): %.2f%%\n" (agreementScore * 100)
    Printf.printf "Consistency (Transitivity): %.2f%%\n" (transitivityScore * 100)

    Monad.unless (Set.null violationsSet) $ do
        Printf.printf "Detected %d transitivity violation(s).\n" violationsCount
        let formattedViolations = map formatViolation (Set.toList violationsSet)
        MonadIO.liftIO $ putStrLn "Violations (showing inconsistent cycle):"
        mapM_ (\(i, v) -> Printf.printf "  %d. %s\n" i v) (zip [1 :: Int ..] formattedViolations)

    Monad.when (Set.null uncomparedSet && Set.null violationsSet) $ do
      putStrLn "All pairs compared and preference set is internally consistent."

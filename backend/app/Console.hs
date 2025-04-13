{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random as Random
import qualified Text.Printf as Printf
import qualified Data.Foldable as Foldable

import Types
import AppState
import Rating
import Scheduler

-- application

main :: IO ()
main = runConsole

runConsole :: IO ()
runConsole = do
  let appToRun :: App ()
      appToRun = do
        user <- setupColourfulUser
        runInteractiveSession user
  runner appToRun

runner :: App a -> IO a
runner app = do
  let initialOptions = colourfulOptions

  (_stateRef, ioRefHandle) <- mkIORefHandle
  let initialConfig = AppConfig
        { configSystemTau = 0.5
        , configStateHandle = ioRefHandle
        , configOptions = initialOptions
        }
  MonadReader.runReaderT app initialConfig

colourfulOptions :: Set.Set Option
colourfulOptions = Set.fromList
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

setupColourfulUser :: App UserId
setupColourfulUser = do
  let user = "coriocactus"
  setupUser user
  pure user

-- event loop

runInteractiveSession :: UserId -> App ()
runInteractiveSession user = do
  MonadIO.liftIO $ putStrLn "Starting ConsoleUI..."
  MonadIO.liftIO $ putStrLn $ "Welcome, " ++ show user ++ "!"
  MonadIO.liftIO $ putStrLn "You will be presented with pairs of options. Choose the one you prefer."
  runEvaluationSession user
  reportFinalStatus user

runEvaluationSession :: UserId -> App ()
runEvaluationSession uid = continueSession (1 :: Int)
  where
    continueSession comparisonNum = do
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      maybePair <- getNextComparisonPair uid

      case maybePair of
        Nothing -> do
          MonadIO.liftIO $ putStrLn ""
          MonadIO.liftIO $ putStrLn ">>> No more comparison pairs available based on current strategy. <<<"

        Just (option1, option2) -> do
          let prevRankings = ratingsToRankings currentRatings
          let prevRatingsMap = ratingsToMap currentRatings

          MonadIO.liftIO $ Printf.printf "\n--- Comparison %d ---\n" comparisonNum
          result <- presentComparison uid option1 option2

          updateRatings uid option1 option2 result

          displayRankings uid prevRankings prevRatingsMap

          continueSession (comparisonNum + 1)

reportFinalStatus :: UserId -> App ()
reportFinalStatus user = do
  optionsList <- Set.toList <$> MonadReader.asks configOptions
  MonadIO.liftIO $ putStrLn "\n--- Final Results ---"
  setupUser user
  finalRankingsWithScores <- getUserRatings user optionsList

  MonadIO.liftIO $ putStrLn $ "Final Glicko ratings for user " ++ show user ++ ":"
  Foldable.forM_ (zip [1 :: Int ..] finalRankingsWithScores) $ \(rank, (option, rating)) ->
    MonadIO.liftIO $ Printf.printf "  %d. %s: %.1f\n" rank (BSC.unpack $ optionName option) rating

  MonadIO.liftIO $ putStrLn "\nEvaluation finished."

-- display

presentComparison :: UserId -> Option -> Option -> App MatchResult
presentComparison uid option1 option2 = do
  swapOrder <- MonadIO.liftIO Random.randomIO :: App Bool
  let (dispOpt1, dispOpt2) =
        if swapOrder then (option2, option1) else (option1, option2)

  MonadIO.liftIO $ do
    putStrLn $ "User: " ++ show uid
    putStrLn "Choose between:"
    Printf.printf "1. %s\n" (BSC.unpack $ optionName dispOpt1)
    Printf.printf "2. %s\n" (BSC.unpack $ optionName dispOpt2)
    putStr "Your choice (1 or 2): "

  getValidChoice swapOrder option1 option2
  where
    getValidChoice :: Bool -> Option -> Option -> App MatchResult
    getValidChoice swapped origOpt1 origOpt2 = do
      choice <- MonadIO.liftIO getLine
      case choice of
        "1" -> pure $ if swapped then Loss else Win
        "2" -> pure $ if swapped then Win else Loss
        _   -> do
          MonadIO.liftIO $ do
            let (dispOpt1, dispOpt2) = if swapped then (origOpt2, origOpt1) else (origOpt1, origOpt2)
            putStrLn "Invalid choice. Please enter 1 or 2."
            Printf.printf "1. %s\n" (BSC.unpack $ optionName dispOpt1)
            Printf.printf "2. %s\n" (BSC.unpack $ optionName dispOpt2)
            putStr "Your choice (1 or 2): "
          getValidChoice swapped origOpt1 origOpt2

displayRankings :: UserId -> [(Option, Int)] -> Map.Map OptionId Double -> App ()
displayRankings uid prevRankings _prevRatingsMap = do
  optionsList <- Set.toList <$> MonadReader.asks configOptions
  setupUser uid
  currentRatingsList <- getUserRatings uid optionsList
  let currentRatingsMap = ratingsToMap currentRatingsList
      prevRankMap = Map.fromList $ map (\(opt, rank) -> (optionId opt, rank)) prevRankings
      currentWithRanks = ratingsToRankings currentRatingsList

      formatChange :: (Option, Int) -> String
      formatChange (option, currentRank) =
        let oid = optionId option
            prevRank = Map.findWithDefault currentRank oid prevRankMap
            currentRating = Map.findWithDefault defaultRating oid currentRatingsMap

            rankChange = prevRank - currentRank

            rankChangeStr = case compare rankChange 0 of
              EQ -> "" :: String
              GT -> Printf.printf " (↑%d)" rankChange
              LT -> Printf.printf " (↓%d)" (abs rankChange)

        in Printf.printf "  %d. %s: %.1f%s" currentRank (BSC.unpack $ optionName option) currentRating rankChangeStr

  MonadIO.liftIO $ do
    putStrLn $ "\nUpdated ratings for user " ++ show uid ++ ":"
    mapM_ (putStrLn . formatChange) currentWithRanks

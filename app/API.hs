{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Set as Set
import qualified GHC.Generics as Generics
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RL

import Servant

import Types
import AppState
import Marshal
import Preference
import Rating
import Scheduler

main :: IO ()
main = runAPI 8080

runAPI :: Int -> IO ()
runAPI port = do
  let appToRun :: App AppConfig
      appToRun = colourfulScaffold
  putStrLn $ "Starting API server on port " ++ show port
  runner port appToRun

runner :: Int -> App AppConfig -> IO ()
runner port app = do
  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let config = AppConfig
        { configKFactor = 32.0
        , configInitialRating = 1500.0
        , configStateRef = initialStateRef
        }
  initialConfig <- MonadReader.runReaderT app config
  Warp.run port (application initialConfig)

colourfulScaffold :: App AppConfig
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

  MonadReader.ask

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: WAI APPLICATION

context :: ErrorFormatters
context = defaultErrorFormatters

application :: AppConfig -> Wai.Application
application cfg = Gzip.gzip Gzip.defaultGzipSettings $ RL.logStdout $
  serveWithContext butler (context :. EmptyContext) (servants cfg)

corsMiddleware :: Wai.Middleware
corsMiddleware = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsOrigins = Just (["http://localhost:3000"], True)
  , Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , Cors.corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
  }

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: API

type API = "api" :> CompareAPI

butler :: Proxy API
butler = Proxy

servants :: AppConfig -> Server API
servants cfg =
  compareServant cfg

runAction :: AppConfig -> App (Either ServerError a) -> Handler a
runAction cfg action = do
  result <- MonadIO.liftIO $ MonadReader.runReaderT action cfg
  case result of
    Left err -> throwError err
    Right val -> pure val

runActionStrict :: AppConfig -> App a -> Handler a
runActionStrict cfg action = MonadIO.liftIO $ MonadReader.runReaderT action cfg

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: COMPARE API

type CompareGetAPI = "compare" :> Capture "userid" UserId :> Get '[JSON] UserSessionData
type ComparePostAPI = "compare" :> Capture "userid" UserId :> ReqBody '[JSON] ComparisonSubmission :> Post '[JSON] UserSessionData

type CompareAPI = EmptyAPI
  :<|> CompareGetAPI
  :<|> ComparePostAPI

compareServant :: AppConfig -> Server CompareAPI
compareServant cfg = emptyServer
  :<|> handleGetCompareData
  :<|> handlePostCompare
  where
    handleGetCompareData :: UserId -> Handler UserSessionData
    handleGetCompareData userId = do
      runAction cfg $ do
        mUserState <- getUserState userId
        case mUserState of
          -- TODO handle auth
          Nothing -> do
            MonadIO.liftIO (putStrLn ("User not found: " ++ show userId))
            pure $ Left err404
          Just _ -> do
            MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show userId
            optionsList <- Set.toList <$> getOptions
            currentRatings <- getUserRatings userId optionsList
            violationsSet <- getViolationsForUser userId
            let violationPairs = findPairsInViolations violationsSet

            mPair <- getNextComparisonPair userId currentRatings violationPairs
            statusData <- gatherStatusData userId currentRatings violationsSet

            MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show userId ++ ": " ++ show mPair
            pure $ Right $ UserSessionData
                { usdNextPair = mPair
                , usdRankings = currentRatings
                , usdStatus   = statusData
                }

    handlePostCompare :: UserId -> ComparisonSubmission -> Handler UserSessionData
    handlePostCompare userId submission = do

      let winnerId = csWinnerId submission
          loserId  = csLoserId submission

      runAction cfg $ do
        mWinnerOpt <- getOptionById winnerId
        mLoserOpt  <- getOptionById loserId

        case (mWinnerOpt, mLoserOpt) of
          (Just winnerOpt, Just loserOpt) -> do
            MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show userId ++ ": " ++ show (optionName winnerOpt) ++ " (Win) vs " ++ show (optionName loserOpt)
            recordComparison userId winnerOpt loserOpt Win
            updateRatings userId winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show userId

            optionsList <- Set.toList <$> getOptions
            updatedRatings <- getUserRatings userId optionsList
            updatedViolationsSet <- getViolationsForUser userId
            let updatedViolationPairs = findPairsInViolations updatedViolationsSet
            updatedPair <- getNextComparisonPair userId updatedRatings updatedViolationPairs
            updatedStatusData <- gatherStatusData userId updatedRatings updatedViolationsSet

            pure $ Right $ UserSessionData
                { usdNextPair = updatedPair
                , usdRankings = updatedRatings
                , usdStatus   = updatedStatusData
                }
          _ -> do
            MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"
            pure $ Left (err400 { errBody = "Invalid Option ID provided" })

getOptionById :: OptionId -> App (Maybe Option)
getOptionById oidToFind = do
  optionsSet <- getOptions
  pure $ List.find (\opt -> optionId opt == oidToFind) (Set.toList optionsSet)

gatherStatusData :: UserId -> [(Option, Double)] -> Set.Set (Option, Option, Option) -> App ComparisonStatus
gatherStatusData uid ratings violationsSet = do
  optionsCount <- Set.size <$> getOptions
  uncomparedSet <- getUncomparedPairsForUser uid

  let totalPossiblePairs = if optionsCount < 2 then 0 else optionsCount * (optionsCount - 1) `div` 2
      completedPairs = totalPossiblePairs - Set.size uncomparedSet
      progressPercent = if totalPossiblePairs == 0 then 100.0 else (fromIntegral completedPairs * 100.0 / fromIntegral totalPossiblePairs :: Double)
      violationsList = Set.toList violationsSet

  agreementScore <- calculateAgreementScore uid ratings
  transitivityScore <- calculateTransitivityScore uid
  isComplete <- checkIfComplete uid

  pure $ ComparisonStatus
    { statusProgress = (completedPairs, totalPossiblePairs, progressPercent)
    , statusAgreement = agreementScore * 100.0
    , statusConsistency = transitivityScore * 100.0
    , statusViolations = violationsList
    , statusIsComplete = isComplete
    }

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | TYPES

data UserSessionData = UserSessionData
  { usdNextPair :: Maybe (Option, Option)
  , usdRankings :: [(Option, Double)]
  , usdStatus   :: ComparisonStatus
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON UserSessionData

data ComparisonStatus = ComparisonStatus
  { statusProgress    :: (Int, Int, Double) -- (completed, total, percent)
  , statusAgreement   :: Double -- percent
  , statusConsistency :: Double -- percent
  , statusViolations  :: [(Option, Option, Option)] -- cycle violations
  , statusIsComplete  :: Bool
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON ComparisonStatus

instance Aeson.FromJSON ComparisonSubmission where
  parseJSON = Aeson.withObject "ComparisonSubmission" $
    \v -> ComparisonSubmission
      <$> v Aeson..: "winnerId"
      <*> v Aeson..: "loserId"

data ComparisonSubmission = ComparisonSubmission
  { csWinnerId :: OptionId
  , csLoserId  :: OptionId
  } deriving (Generics.Generic, Show)

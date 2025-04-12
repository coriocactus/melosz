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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
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

import Auth
import Redis
import Runner

main :: IO ()
main = launch 8080

launch :: Int -> IO ()
launch port = do
  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let config = AppConfig
        { configKFactor = 32.0
        , configInitialRating = 1500.0
        , configStateRef = initialStateRef
        }
  initialConfig <- MonadReader.runReaderT app config

  pool <- mkRedisPool

  putStrLn $ "=== === === Running melosz backend === === ==="
  putStrLn $ "Listening: http://localhost:" ++ show port

  Warp.run port (application initialConfig pool)
  where 
    app :: App AppConfig
    app = colourfulScaffold

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

corsMiddleware :: Wai.Middleware
corsMiddleware = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsOrigins = Just (["http://localhost:3000"], True)
  , Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , Cors.corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
  }

application :: AppConfig -> RedisPool -> Wai.Application
application cfg pool = corsMiddleware $
  Gzip.gzip Gzip.defaultGzipSettings $ RL.logStdout $
  serveWithContext butler underButler (servants cfg pool)

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = jsonErrorFormatter
  , urlParseErrorFormatter = jsonErrorFormatter
  , headerParseErrorFormatter = jsonErrorFormatter
  , notFoundErrorFormatter = jsonNotFoundErrorFormatter
  }

jsonErrorFormatter :: ErrorFormatter
jsonErrorFormatter typeRep req errMsg =
  let body = Aeson.encode $ Aeson.object
        [ "error" Aeson..= errMsg
        , "source" Aeson..= show typeRep
        , "request_path" Aeson..= TextEnc.decodeUtf8 (Wai.rawPathInfo req)
        , "method" Aeson..= TextEnc.decodeUtf8 (Wai.requestMethod req)
        ]
  in err400 { errBody = body, errHeaders = [("Content-Type", "application/json;charset=utf-8")] }

jsonNotFoundErrorFormatter :: NotFoundErrorFormatter
jsonNotFoundErrorFormatter req =
  let body = Aeson.encode $ Aeson.object
        [ "error" Aeson..= ("Not Found" :: Text.Text)
        , "request_path" Aeson..= TextEnc.decodeUtf8 (Wai.rawPathInfo req)
        ]
  in err404 { errBody = body, errHeaders = [("Content-Type", "application/json;charset=utf-8")] }

underButler :: Context '[ErrorFormatters]
underButler = jsonErrorFormatters :. EmptyContext

butler :: Proxy API
butler = Proxy

type AuthHeader = Text.Text
type Protect = Header "authorization" AuthHeader

type API = AuthAPI
  :<|> Protect :> CompareAPI

servants :: AppConfig -> RedisPool -> Server API
servants cfg pool = authServant pool
  :<|> compareServant cfg pool

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: COMPARE API

data UserSession = UserSession
  { usNextPair :: Maybe (Option, Option)
  , usRankings :: [(Option, Double)]
  , usStatus   :: ComparisonStatus
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON UserSession

data ComparisonStatus = ComparisonStatus
  { statusProgress    :: (Int, Int, Double)         -- completed, total, percent
  , statusAgreement   :: Double                     -- percent
  , statusConsistency :: Double                     -- percent
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

type CompareGetAPI = "compare" :> Get '[JSON] UserSession
type ComparePostAPI = "compare" :> ReqBody '[JSON] ComparisonSubmission :> Post '[JSON] UserSession

type CompareTestAPI = "compare" :> "test" :> Get '[JSON] NoContent

type CompareAPI = EmptyAPI
  :<|> CompareGetAPI
  :<|> ComparePostAPI
  :<|> CompareTestAPI

extractHash :: Text.Text -> Maybe Text.Text
extractHash txt = Text.stripPrefix (Text.pack "MELOSZ ") txt

-- TODO TEMP USERS FOR GUEST ACCESS
initUser :: Maybe AuthHeader -> UserId
initUser maybeAuth = case maybeAuth of
  Just auth -> case extractHash auth of
    Just hash -> UserId $ TextEnc.encodeUtf8 hash
    Nothing -> UserId "temp-x"
  Nothing -> UserId "temp-y"

compareServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server CompareAPI
compareServant cfg _pool maybeAuth = 
  let userId = initUser maybeAuth
  in emptyServer
  :<|> handleGetCompareData userId
  :<|> handlePostCompare userId
  :<|> handleTestCompare userId
  where
    handleTestCompare :: UserId -> Handler NoContent
    handleTestCompare userId = do
      MonadIO.liftIO $ putStrLn $ "USER: " ++ show userId
      run $ Right NoContent

    handleGetCompareData :: UserId -> Handler UserSession
    handleGetCompareData userId =
      runApp cfg $ do
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
            pure $ Right $ UserSession
                { usNextPair = mPair
                , usRankings = currentRatings
                , usStatus   = statusData
                }

    handlePostCompare :: UserId -> ComparisonSubmission -> Handler UserSession
    handlePostCompare userId submission = do

      let winnerId = csWinnerId submission
          loserId  = csLoserId submission

      runApp cfg $ do
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

            pure $ Right $ UserSession
                { usNextPair = updatedPair
                , usRankings = updatedRatings
                , usStatus   = updatedStatusData
                }
          _ -> do
            MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"
            pure $ Left (err400 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Invalid Option ID provided" :: Text.Text)]) })

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Aeson as Aeson
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
import Rating
import Scheduler

import Actions
import Redis
import Auth

-- application

main :: IO ()
main = launch 8080

launch :: Int -> IO ()
launch port = do
  let options = colourfulOptions

  pool <- mkRedisPool
  let redisHandle = mkRedisHandle pool options

  let config = AppConfig
        { configOptions = options
        , configSystemTau = 0.5
        , configStateHandle = redisHandle
        }

  putStrLn $ "=== === === Running melosz backend (API - Redis) === === ==="
  putStrLn $ "Listening: http://localhost:" ++ show port

  Warp.run port (application config pool)

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

-- webserver

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
  :<|> compareServant cfg

-- comparison

data UserSession = UserSession
  { usNextPair :: Maybe (Option, Option)
  , usRankings :: [(Option, Double)]
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON UserSession

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

initUser :: AppConfig -> Maybe AuthHeader -> UserId
initUser _cfg maybeAuth = do
  case maybeAuth of
    Just auth -> case extractHash auth of
      Just hash -> UserId $ TextEnc.encodeUtf8 hash
      Nothing -> UserId "temp-invalid-auth"
    Nothing -> UserId "temp-guest"

compareServant :: AppConfig -> Maybe AuthHeader -> Server CompareAPI
compareServant cfg maybeAuth =
  let uid = initUser cfg maybeAuth
  in emptyServer
  :<|> handleGetCompareData uid
  :<|> handlePostCompare uid
  :<|> handleTestCompare uid
  where
    handleTestCompare :: UserId -> Handler NoContent
    handleTestCompare uid = do
      MonadIO.liftIO $ putStrLn $ "Test endpoint accessed by USER: " ++ show uid
      pure NoContent

    fetchAndBuildSession :: UserId -> App (Either ServerError UserSession)
    fetchAndBuildSession uid = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      mPair <- getNextComparisonPair uid

      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show uid ++ ": " ++ show mPair
      pure $ Right $ UserSession
        { usNextPair = mPair
        , usRankings = currentRatings
        }

    handleGetCompareData :: UserId -> Handler UserSession
    handleGetCompareData userId =
      execApp cfg $ fetchAndBuildSession userId

    handlePostCompare :: UserId -> ComparisonSubmission -> Handler UserSession
    handlePostCompare userId submission = do
      let winnerId = csWinnerId submission
          loserId  = csLoserId submission

      execApp cfg $ do
        mWinnerOpt <- getOptionById winnerId
        mLoserOpt  <- getOptionById loserId

        case (mWinnerOpt, mLoserOpt) of
          (Just winnerOpt, Just loserOpt) -> do
            MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show userId ++ ": " ++ show (optionName winnerOpt) ++ " (Win) vs " ++ show (optionName loserOpt)
            updateRatings userId winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show userId
            fetchAndBuildSession userId
          _ -> do
            MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"
            pure $ Left (err400 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Invalid Option ID provided" :: Text.Text)]) })

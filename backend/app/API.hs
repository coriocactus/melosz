{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Aeson as Aeson
import qualified Data.Base64.Types as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified System.Random as Random

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
  putStrLn $ "=== === === Running server === === ==="
  putStrLn $ "Listening: http://localhost:" ++ show port
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

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = jsonErrorFormatter
  , urlParseErrorFormatter = jsonErrorFormatter
  , headerParseErrorFormatter = jsonErrorFormatter
  , notFoundErrorFormatter = jsonNotFoundErrorFormatter
  }

underButler :: Context '[ErrorFormatters]
underButler = jsonErrorFormatters :. EmptyContext

application :: AppConfig -> Wai.Application
application cfg = corsMiddleware $
  Gzip.gzip Gzip.defaultGzipSettings $ RL.logStdout $
  serveWithContext butler underButler (servants cfg)

corsMiddleware :: Wai.Middleware
corsMiddleware = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsOrigins = Just (["http://localhost:3000"], True)
  , Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , Cors.corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
  }

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: API

butler :: Proxy API
butler = Proxy

type API = EmptyAPI
  :<|> AuthAPI
  :<|> CompareAPI

servants :: AppConfig -> Server API
servants cfg = emptyServer
  :<|> authServant cfg
  :<|> compareServant cfg

runAction :: AppConfig -> App (Either ServerError a) -> Handler a
runAction cfg action = do
  result <- MonadIO.liftIO $ MonadReader.runReaderT action cfg
  case result of
    Left err -> throwError err
    Right val -> pure val

runActionStrict :: AppConfig -> App a -> Handler a
runActionStrict cfg action = MonadIO.liftIO $ MonadReader.runReaderT action cfg

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: AUTH API

type AuthToken = Text.Text
type AuthHash = Text.Text

data AuthInit = AuthInit
  { authInitToken :: AuthToken
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON AuthInit where
  toJSON = Aeson.genericToJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \x -> if x == "authInitToken" then "token" else x
    }

instance Aeson.FromJSON AuthPayload where
  parseJSON = Aeson.withObject "AuthPayload" $
    \v -> AuthPayload
      <$> v Aeson..: "token"
      <*> v Aeson..: "email"
data AuthPayload = AuthPayload
  { authPayloadToken :: AuthToken
  , authEmail :: Text.Text
  } deriving (Generics.Generic, Show)

data AuthTerm = AuthTerm
  { authHash :: AuthHash
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON AuthTerm where
  toJSON = Aeson.genericToJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \x -> if x == "authHash" then "hash" else x
    }

type LoginGetAPI = "login" :> Get '[JSON] AuthInit
type LoginPostAPI = "login" :> ReqBody '[JSON] AuthPayload :> PostNoContent
type RegisterGetAPI = "register" :> Get '[JSON] AuthInit
type RegisterPostAPI = "register" :> ReqBody '[JSON] AuthPayload :> PostNoContent
type AuthGetAPI = "auth" :> Capture "hash" AuthHash :> Get '[JSON] NoContent

type AuthAPI = EmptyAPI
  :<|> LoginGetAPI
  :<|> LoginPostAPI
  :<|> RegisterGetAPI
  :<|> RegisterPostAPI
  :<|> AuthGetAPI

authServant :: AppConfig -> Server AuthAPI
authServant cfg = emptyServer
  :<|> handleGetLogin
  :<|> handlePostLogin
  :<|> handleGetRegister
  :<|> handlePostRegister
  :<|> handleGetAuth
  where
    handleGetLogin :: Handler AuthInit
    handleGetLogin = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["123", "456", "789"]
      runAction cfg $ do
        pure $ Right $ AuthInit
          { authInitToken = token
          }

    handlePostLogin :: AuthPayload -> Handler NoContent
    handlePostLogin payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["123", "456", "789"]
      case validateToken now (authPayloadToken payload) tokenState of
        Right () -> do
          nonce <- BS.pack <$> Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))
          let hash = generateAuthHash now nonce (authEmail payload)
          MonadIO.liftIO $ putStrLn $ "Complete Login: " ++ show ("http://localhost:8080/auth/" <> hash)
          runAction cfg $ pure $ Right NoContent
        Left err -> do
          MonadIO.liftIO $ putStrLn $ "Error: " ++ show err
          runAction cfg $ pure $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= (show err :: String)]) })

    handleGetRegister :: Handler AuthInit
    handleGetRegister = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["987", "654", "321"]
      runAction cfg $ do
        pure $ Right $ AuthInit
          { authInitToken = token
          }

    handlePostRegister :: AuthPayload -> Handler NoContent
    handlePostRegister payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["987", "654", "321"]
      case validateToken now (authPayloadToken payload) tokenState of
        Right () -> do
          nonce <- BS.pack <$> Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))
          let hash = generateAuthHash now nonce (authEmail payload)
          MonadIO.liftIO $ putStrLn $ "Complete Registration: " ++ show ("http://localhost:8080/auth/" <> hash)
          runAction cfg $ pure $ Right NoContent
        Left err -> do
          MonadIO.liftIO $ putStrLn $ "Error: " ++ show err
          runAction cfg $ pure $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= (show err :: String)]) })

    handleGetAuth :: AuthToken -> Handler NoContent
    handleGetAuth _hash = do
      runAction cfg $ pure $ Right NoContent

generateAuthHash :: POSIXTime.POSIXTime -> BS.ByteString -> Text.Text -> Text.Text
generateAuthHash now nonce email = Text.pack $ SHA.showDigest hash2
  where
    now' = BSL.fromStrict $ BSU.fromString $ show (floor now :: Int)
    nonce' = BSL.fromStrict nonce
    email' = BSL.fromStrict $ TextEnc.encodeUtf8 email
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    hash1 = SHA.sha512 $ BSL.concat [ nonce' , email' , now', secret ]
    hash1' = BSL.fromStrict $ BSU.fromString $ SHA.showDigest hash1
    hash2 = SHA.hmacSha512 secret hash1'

type TokenState = [BS.ByteString]

encodeBS :: BS.ByteString -> Text.Text
encodeBS bs = Base64.extractBase64 $ Base64BS.encodeBase64 bs

decodeTxt :: Text.Text -> Either Text.Text BS.ByteString
decodeTxt txt = Base64BS.decodeBase64Untyped $ TextEnc.encodeUtf8 txt

generateToken :: POSIXTime.POSIXTime -> TokenState -> Text.Text
generateToken now state = Text.intercalate "." encodedToken
  where
    tokenData = BSL.concat (map BSL.fromStrict state)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    signature = SHA.showDigest $ SHA.hmacSha256 secret tokenData
    expiry = show (floor $ now + 60 * 5 :: Int)
    token = state ++ (map BSU.fromString [expiry,  signature])
    encodedToken = map (encodeBS) token

validateToken :: POSIXTime.POSIXTime -> Text.Text -> TokenState -> Either Text.Text ()
validateToken now token expectedState = do
  parts <- validateTokenFormat token expectedState
  (state, expiry, signature) <- extractTokenParts parts expectedState
  expiry' <- parseExpiry expiry
  validateExpiry now expiry'
  decodedState <- mapM decodeTxt state
  validateState decodedState expectedState
  decodedSignature <- decodeTxt signature
  validateSignature decodedSignature expectedState

validateTokenFormat :: Text.Text -> [BS.ByteString] -> Either Text.Text [Text.Text]
validateTokenFormat token expectedState =
  if length parts == expectedLength
    then Right parts
    else Left "Invalid token format"
  where
    parts = Text.splitOn "." token
    expectedLength = length expectedState + 2  -- state + expiry + signature

extractTokenParts :: [Text.Text] -> [BS.ByteString] -> Either Text.Text ([Text.Text], Text.Text, Text.Text)
extractTokenParts parts expectedState =
  case rest of
    [expiry, signature] -> Right (state, expiry, signature)
    _ -> Left "Invalid token parts"
  where
    (state, rest) = splitAt (length expectedState) parts

parseExpiry :: Text.Text -> Either Text.Text Int
parseExpiry expiry = case decodeTxt expiry of
  Right decoded -> case BSC.readInt decoded of
    Just (expiry', _) -> Right expiry'
    Nothing -> Left "Invalid expiry format"
  Left err -> Left err

validateExpiry :: POSIXTime.POSIXTime -> Int -> Either Text.Text ()
validateExpiry now expiry =
  if fromIntegral expiry >= now
    then Right ()
    else Left "Token expired"

validateState :: [BS.ByteString] -> [BS.ByteString] -> Either Text.Text ()
validateState decodedState expectedState =
  if and $ zipWith (==) decodedState expectedState
    then Right ()
    else Left "State validation failed"

validateSignature :: BS.ByteString -> [BS.ByteString] -> Either Text.Text ()
validateSignature signature expectedState =
  if signature == BSU.fromString expectedSignature
    then Right ()
    else Left "Invalid signature"
  where
    tokenData = BSL.concat (map BSL.fromStrict expectedState)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    expectedSignature = SHA.showDigest $ SHA.hmacSha256 secret tokenData

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

type CompareGetAPI = "compare" :> Capture "userid" UserId :> Get '[JSON] UserSession
type ComparePostAPI = "compare" :> Capture "userid" UserId :> ReqBody '[JSON] ComparisonSubmission :> Post '[JSON] UserSession

type CompareAPI = EmptyAPI
  :<|> CompareGetAPI
  :<|> ComparePostAPI

compareServant :: AppConfig -> Server CompareAPI
compareServant cfg = emptyServer
  :<|> handleGetCompareData
  :<|> handlePostCompare
  where
    handleGetCompareData :: UserId -> Handler UserSession
    handleGetCompareData userId =
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
            pure $ Right $ UserSession
                { usNextPair = mPair
                , usRankings = currentRatings
                , usStatus   = statusData
                }

    handlePostCompare :: UserId -> ComparisonSubmission -> Handler UserSession
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

            pure $ Right $ UserSession
                { usNextPair = updatedPair
                , usRankings = updatedRatings
                , usStatus   = updatedStatusData
                }
          _ -> do
            MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"
            pure $ Left (err400 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Invalid Option ID provided" :: String)]) })

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

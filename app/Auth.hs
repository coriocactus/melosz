{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Auth where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Base64.Types as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified System.Random as Random
import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Web.FormUrlEncoded as Form

import Servant

import Types

import Redis
import Actions
import Templates

setClientHash :: H.Html -> Hash -> Authenticated
setClientHash html hash = addHeader cookie html
  where cookie = mkCookie hash

mkCookie :: Text.Text -> Text.Text
mkCookie hash = Text.concat
  [ "x-auth-hash=", hash
  , "; Path=/"
  , "; Secure"
  , "; HttpOnly"
  , "; SameSite=Strict"
  , "; Max-Age=", Text.pack $ show (10 * 365 * 24 * 60 * 60 :: Int) -- "permanent"
  ]

type AuthHeader = Text.Text
type Protect = Header "cookie" AuthHeader

initUser :: RedisPool -> Maybe AuthHeader -> Text.Text -> Handler (UserId, Bool)
initUser pool maybeAuth destination =
  case maybeAuth of
    Nothing -> handleMissingAuth pool destination
    Just auth -> case extractHash auth of
      Nothing -> handleMissingAuth pool destination
      Just hash -> do
        maybeUserId <- getUserByHash pool hash
        case maybeUserId of
          Nothing -> handleMissingAuth pool destination
          Just (isRegistered, userId) -> do
            case isRegistered of
              True -> pure (userId, True)
              False -> extendGuest pool userId hash >> pure (userId, False)

handleMissingAuth :: RedisPool -> Text.Text -> Handler a
handleMissingAuth pool destination = do
  now <- MonadIO.liftIO POSIXTime.getPOSIXTime
  nonce <- MonadIO.liftIO generateNonce
  uuid <- MonadIO.liftIO $ UUID.toString <$> UUID.nextRandom
  let email = encodeString uuid
      hash = generateAuthHash now nonce $ TextEnc.decodeUtf8 email
  _ <- execRedis pool $ makeGuestRedis email (encodeText hash)
  _ <- extendGuest pool (UserId email) hash
  exec $ Left $ err307
    { errHeaders =
      [ ("location", TextEnc.encodeUtf8 destination)
      , ("set-cookie", TextEnc.encodeUtf8 $ mkCookie hash)
      ]
    }

extractHash :: Text.Text -> Maybe Text.Text
extractHash txt = Text.stripPrefix (Text.pack "x-auth-hash=") txt

getUserByHash :: RedisPool -> Hash -> Handler (Maybe (Bool, UserId))
getUserByHash pool hash = do
  result <- execRedis pool $ getUserByHashRedis hash
  case result of
    Left err -> handleRedisError err
    Right maybeUserId -> pure maybeUserId

extendGuest :: RedisPool -> UserId -> Text.Text -> Handler (Redis.TxResult ())
extendGuest pool uid hash = execRedis pool $ extendGuestRedis uid (encodeText hash)

type Token = Text.Text
type Hash = Text.Text
type TokenState = [BS.ByteString]

data AuthPayload = AuthPayload
  { authPayloadToken :: Token
  , authEmail :: Text.Text
  } deriving (Show)

instance Form.FromForm AuthPayload where
  fromForm form = AuthPayload
    <$> Form.parseUnique "token" form
    <*> Form.parseUnique "email" form

type LoginGetAPI = "login" :> Get '[ServantBlaze.HTML] H.Html
type RegisterGetAPI = "register" :> Get '[ServantBlaze.HTML] H.Html

type LoginPostAPI = "login" :> ReqBody '[FormUrlEncoded] AuthPayload :> Post '[ServantBlaze.HTML] H.Html
type RegisterPostAPI = "register" :> ReqBody '[FormUrlEncoded] AuthPayload :> Post '[ServantBlaze.HTML] H.Html

type Authenticated = Headers '[Header "Set-Cookie" Text.Text] H.Html
type AuthGetAPI = "auth" :> Capture "token" Token :> Get '[ServantBlaze.HTML] Authenticated
type GuestGetAPI = "guest" :> Get '[ServantBlaze.HTML] Authenticated

type AuthAPI = EmptyAPI
  :<|> LoginGetAPI
  :<|> RegisterGetAPI
  :<|> LoginPostAPI
  :<|> RegisterPostAPI
  :<|> AuthGetAPI

authServant :: RedisPool -> Server AuthAPI
authServant pool = emptyServer
  :<|> handleGetLogin
  :<|> handleGetRegister
  :<|> handlePostLogin
  :<|> handlePostRegister
  :<|> handleGetAuth
  where
    handleGetRegister :: Handler H.Html
    handleGetRegister = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["987", "654", "321"]
      exec $ Right $ mkRegistrationPage token

    handleGetLogin :: Handler H.Html
    handleGetLogin = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["123", "456", "789"]
      exec $ Right $ mkLoginPage token

    handlePostRegister :: AuthPayload -> Handler H.Html
    handlePostRegister payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["987", "654", "321"]

      case validateToken now (authPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          let email = encodeText $ authEmail payload

          isRegistered <- execRedis pool $ findUserByEmailRedis email
          case isRegistered of
            Left err -> handleRedisError err
            Right True -> handleDuplicateRegistration email
            Right False -> do
              let token = encodeText $ generateToken now [ "123", "234", "345", email ]
              _ <- execRedis pool $ setAuthTokenRedis token email

              -- TODO SEND EMAIL
              MonadIO.liftIO $ putStrLn $ "Registration confirmation: " ++ show ("http://localhost:5002/auth/" <> token)
              exec $ Right emailSentMessage

    handlePostLogin :: AuthPayload -> Handler H.Html
    handlePostLogin payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["123", "456", "789"]

      case validateToken now (authPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          let email = encodeText $ authEmail payload

          isRegistered <- execRedis pool $ findUserByEmailRedis email
          case isRegistered of
            Left err -> handleRedisError err
            Right False -> handlePhantomLogin email
            Right True -> do
              let token = encodeText $ generateToken now [ "123", "234", "345", email ]
              _ <- execRedis pool $ setAuthTokenRedis token email

              -- TODO SEND EMAIL
              MonadIO.liftIO $ putStrLn $ "Login confirmation: " ++ show ("http://localhost:5002/auth/" <> token)
              exec $ Right emailSentMessage

    handleGetAuth :: Token -> Handler Authenticated
    handleGetAuth token = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime

      maybeEmail <- execRedis pool $ getAuthTokenRedis (encodeText token)
      case maybeEmail of
        Left err -> handleRedisError err
        Right Nothing -> handleTokenNotFound
        Right (Just email) -> do
          _ <- execRedis pool $ delAuthTokenRedis (encodeText token)
          let tokenState = ["123", "234", "345", email]

          case validateToken now token tokenState of
            Left err -> handleValidationError err
            Right () -> do
              nonce <- MonadIO.liftIO generateNonce
              let hash = generateAuthHash now nonce $ TextEnc.decodeUtf8 email

              isRegistered <- execRedis pool $ findUserByEmailRedis email
              case isRegistered of
                Left err -> handleRedisError err
                Right True -> do
                  MonadIO.liftIO $ putStrLn $ "Login: " ++ show email
                  _ <- execRedis pool $ loginRedis (encodeTimestamp now) email (encodeText hash)
                  exec $ Right $ setClientHash authenticatedMessage hash

                Right False -> do
                  MonadIO.liftIO $ putStrLn $ "Registration: " ++ show email
                  _ <- execRedis pool $ registerRedis (encodeTimestamp now) email (encodeText hash)
                  exec $ Right $ setClientHash authenticatedMessage hash

handleDuplicateRegistration :: BS.ByteString -> Handler a
handleDuplicateRegistration email = do
  MonadIO.liftIO $ putStrLn $ "Email already registered: " ++ show email
  exec $ Left (err409 { errBody = R.renderHtml template})
  where template = fastMessage "email already registered" ("/login", "login")

handlePhantomLogin :: BS.ByteString -> Handler a
handlePhantomLogin email = do
  MonadIO.liftIO $ putStrLn $ "User not found: " ++ show email
  exec $ Left (err409 { errBody = R.renderHtml template})
  where template = fastMessage "user not found" ("/register", "register")

handleTokenNotFound :: Handler a
handleTokenNotFound = do
  MonadIO.liftIO $ putStrLn $ "Authentication link not found"
  exec $ Left (err404 { errBody = R.renderHtml template})
  where template = fastMessage "invalid authentication" ("/login", "try again")

handleValidationError :: Text.Text -> Handler a
handleValidationError err = do
  MonadIO.liftIO $ putStrLn $ "Validation Error: " <> show err
  exec $ Left (err401 { errBody = R.renderHtml template})
  where template = fastMessage "invalid token" ("/", "home")

handleRedisError :: Redis.Reply -> Handler a
handleRedisError err = do
  MonadIO.liftIO $ putStrLn $ "Redis Error: " <> show err
  exec $ Left (err500 { errBody = R.renderHtml template})
  where template = fastMessage "internal server error" ("/", "home")

encodeBase64BS :: BS.ByteString -> Text.Text
encodeBase64BS bs = Base64.extractBase64 $ Base64BS.encodeBase64 bs

decodeBase64Text :: Text.Text -> Either Text.Text BS.ByteString
decodeBase64Text txt = Base64BS.decodeBase64Untyped $ TextEnc.encodeUtf8 txt

generateNonce :: IO BS.ByteString
generateNonce = BS.pack <$> Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))

generateAuthHash :: POSIXTime.POSIXTime -> BS.ByteString -> Text.Text -> Hash
generateAuthHash now nonce email = Text.pack $ SHA.showDigest hash2
  where
    now' = BSL.fromStrict $ encodeString $ show (floor now :: Int)
    nonce' = BSL.fromStrict nonce
    email' = BSL.fromStrict $ TextEnc.encodeUtf8 email
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    hash1 = SHA.sha512 $ BSL.concat [ nonce' , email' , now', secret ]
    hash1' = BSL.fromStrict $ encodeString $ SHA.showDigest hash1
    hash2 = SHA.hmacSha512 secret hash1'

generateToken :: POSIXTime.POSIXTime -> TokenState -> Token
generateToken now state = Text.intercalate "." encodedToken
  where
    tokenData = BSL.concat (map BSL.fromStrict state)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    signature = SHA.showDigest $ SHA.hmacSha256 secret tokenData
    expiry = show (floor $ now + 60 * 5 :: Int)
    token = state ++ (map encodeString [expiry,  signature])
    encodedToken = map (encodeBase64BS) token

validateToken :: POSIXTime.POSIXTime -> Text.Text -> TokenState -> Either Text.Text ()
validateToken now token expectedState = do
  parts <- validateTokenFormat token expectedState
  (state, expiry, signature) <- extractTokenParts parts expectedState
  expiry' <- parseExpiry expiry
  validateExpiry now expiry'
  decodedState <- mapM decodeBase64Text state
  validateState decodedState expectedState
  decodedSignature <- decodeBase64Text signature
  validateSignature decodedSignature expectedState

validateTokenFormat :: Text.Text -> TokenState -> Either Text.Text [Text.Text]
validateTokenFormat token expectedState =
  if length parts == expectedLength
    then Right parts
    else Left "Invalid token format"
  where
    parts = Text.splitOn "." token
    expectedLength = length expectedState + 2  -- state + expiry + signature

extractTokenParts :: [Text.Text] -> TokenState -> Either Text.Text ([Text.Text], Text.Text, Text.Text)
extractTokenParts parts expectedState =
  case rest of
    [expiry, signature] -> Right (state, expiry, signature)
    _ -> Left "Invalid token parts"
  where
    (state, rest) = splitAt (length expectedState) parts

parseExpiry :: Text.Text -> Either Text.Text Int
parseExpiry expiry = case decodeBase64Text expiry of
  Right decoded -> case BSC.readInt decoded of
    Just (expiry', _) -> Right expiry'
    Nothing -> Left "Invalid expiry format"
  Left err -> Left err

validateExpiry :: POSIXTime.POSIXTime -> Int -> Either Text.Text ()
validateExpiry now expiry =
  if fromIntegral expiry >= now
    then Right ()
    else Left "Token expired"

validateState :: TokenState -> TokenState -> Either Text.Text ()
validateState decodedState expectedState =
  if and $ zipWith (==) decodedState expectedState
    then Right ()
    else Left "State validation failed"

validateSignature :: BS.ByteString -> TokenState -> Either Text.Text ()
validateSignature signature expectedState =
  if signature == encodeString expectedSignature
    then Right ()
    else Left "Invalid signature"
  where
    tokenData = BSL.concat (map BSL.fromStrict expectedState)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    expectedSignature = SHA.showDigest $ SHA.hmacSha256 secret tokenData

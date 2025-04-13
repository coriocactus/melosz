{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Aeson as Aeson
import qualified Data.Base64.Types as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified Database.Redis as Redis
import qualified GHC.Generics as Generics
import qualified System.Random as Random

import Servant

import Actions
import Redis

type Token = Text.Text
type Hash = Text.Text
type TokenState = [BS.ByteString]

data AuthToken = AuthToken
  { authInitToken :: Token
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON AuthToken where
  toJSON = Aeson.genericToJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \x -> if x == "authInitToken" then "token" else x
    }

data AuthHash = AuthHash
  { authHash :: Hash
  } deriving (Generics.Generic, Show)
instance Aeson.ToJSON AuthHash where
  toJSON = Aeson.genericToJSON $ Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \x -> if x == "authHash" then "hash" else x
    }

instance Aeson.FromJSON AuthPayload where
  parseJSON = Aeson.withObject "AuthPayload" $
    \v -> AuthPayload
      <$> v Aeson..: "token"
      <*> v Aeson..: "email"
data AuthPayload = AuthPayload
  { authPayloadToken :: Token
  , authEmail :: Text.Text
  } deriving (Generics.Generic, Show)

type LoginGetAPI = "login" :> Get '[JSON] AuthToken
type RegisterGetAPI = "register" :> Get '[JSON] AuthToken
type LoginPostAPI = "login" :> ReqBody '[JSON] AuthPayload :> PostNoContent
type RegisterPostAPI = "register" :> ReqBody '[JSON] AuthPayload :> PostNoContent
type AuthGetAPI = "auth" :> Capture "token" Token :> Get '[JSON] AuthHash

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
    handleGetRegister :: Handler AuthToken
    handleGetRegister = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["987", "654", "321"]
      exec $ Right $ AuthToken { authInitToken = token }

    handleGetLogin :: Handler AuthToken
    handleGetLogin = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["123", "456", "789"]
      exec $ Right $ AuthToken { authInitToken = token }

    handlePostRegister :: AuthPayload -> Handler NoContent
    handlePostRegister payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["987", "654", "321"]

      case validateToken now (authPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          let email = encodeText $ authEmail payload

          eitherEmailExists <- execRedis pool $ findUserRedis email
          case eitherEmailExists of
            Left err -> handleRedisError err
            Right True -> handleDuplicateRegistration email
            Right False -> do
              let token = encodeText $ generateToken now [ "123", "234", "345", email ]
              _ <- execRedis pool $ setAuthTokenRedis token email

              MonadIO.liftIO $ putStrLn $ "Registration confirmation: " ++ show ("http://localhost:8080/auth/" <> token)
              exec $ Right NoContent

    handlePostLogin :: AuthPayload -> Handler NoContent
    handlePostLogin payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["123", "456", "789"]

      case validateToken now (authPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          let email = encodeText $ authEmail payload

          eitherEmailExists <- execRedis pool $ findUserRedis email
          case eitherEmailExists of
            Left err -> handleRedisError err
            Right False -> handlePhantomLogin email
            Right True -> do
              let token = encodeText $ generateToken now [ "123", "234", "345", email ]
              _ <- execRedis pool $ setAuthTokenRedis token email

              MonadIO.liftIO $ putStrLn $ "Login confirmation: " ++ show ("http://localhost:8080/auth/" <> token)
              exec $ Right NoContent

    handleGetAuth :: Token -> Handler AuthHash
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

              eitherEmailExists <- execRedis pool $ findUserRedis email
              case eitherEmailExists of
                Left err -> handleRedisError err
                Right True -> do
                  MonadIO.liftIO $ putStrLn $ "Login: " ++ show email
                  _ <- execRedis pool $ loginRedis (encodeTimestamp now) email (encodeText hash)
                  exec $ Right $ AuthHash { authHash = hash }

                Right False -> do
                  MonadIO.liftIO $ putStrLn $ "Registration: " ++ show email
                  _ <- execRedis pool $ registerRedis (encodeTimestamp now) email (encodeText hash)
                  exec $ Right $ AuthHash { authHash = hash }

    handleDuplicateRegistration :: BS.ByteString -> Handler a
    handleDuplicateRegistration email = do
      MonadIO.liftIO $ putStrLn $ "Email already registered: " ++ show email
      exec $ Left (err409 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Email already registered" :: Text.Text)]) })

    handlePhantomLogin :: BS.ByteString -> Handler a
    handlePhantomLogin email = do
      MonadIO.liftIO $ putStrLn $ "User not found: " ++ show email
      exec $ Left (err409 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("User not found" :: Text.Text)]) })

    handleTokenNotFound :: Handler a
    handleTokenNotFound = do
      MonadIO.liftIO $ putStrLn $ "Authentication link not found"
      exec $ Left (err404 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Authentication link not found" :: Text.Text)]) })

    handleValidationError :: Text.Text -> Handler a
    handleValidationError err = do
      MonadIO.liftIO $ putStrLn $ "Validation Error: " <> show err
      exec $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Validation Error: " <> show err)]) })

    handleRedisError :: Redis.Reply -> Handler a
    handleRedisError err = do
      MonadIO.liftIO $ putStrLn $ "Redis Error: " <> show err
      exec $ Left (err500 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Redis Error: " <> show err)]) })

----

-- | Redis Schema:
-- [hash] tokens: [token -> email]
-- [hash] hashes: [hash -> email]
-- [set] users -> [email]
-- [hash] [email] -> [hash, created_on, accessed_on]

userMetadataKey :: BS.ByteString -> BS.ByteString
userMetadataKey email = "user:" <> email

encodeText :: Text.Text -> BS.ByteString
encodeText text = TextEnc.encodeUtf8 text

encodeTimestamp :: POSIXTime.POSIXTime -> BS.ByteString
encodeTimestamp now = BSU.fromString $ show (floor now :: Int)

hExpire :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Redis.Redis (Either Redis.Reply Redis.Status)
hExpire key seconds field = Redis.sendRequest ["HEXPIRE", key, seconds, "FIELDS", "1", field]

setAuthTokenRedis :: BS.ByteString -> BS.ByteString -> Redis.Redis (Redis.TxResult ())
setAuthTokenRedis token email = Redis.multiExec $ do
  _ <- Redis.hset "tokens" token email
  _ <- Redis.liftRedis $ hExpire "tokens" token "300"
  return $ pure ()

getAuthTokenRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f (Maybe BSU.ByteString))
getAuthTokenRedis token = Redis.hget "tokens" token

delAuthTokenRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f Integer)
delAuthTokenRedis token = Redis.hdel "tokens" [token]

findUserRedis :: Redis.RedisCtx m f => BSU.ByteString -> m (f Bool)
findUserRedis email = Redis.sismember "users" email

loginRedis :: BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
loginRedis now email hash = Redis.multiExec $ do
  _ <- Redis.hset "hashes" hash email
  _ <- Redis.hset (userMetadataKey email) "hash" hash
  _ <- Redis.hset (userMetadataKey email) "accessed_on" now
  return $ pure ()

-- hashes: hgetall from hashes and filter out email
_logoutRedis :: Redis.RedisCtx m f => [BSU.ByteString] -> m (f Integer)
_logoutRedis hashes = Redis.hdel "hashes" hashes

registerRedis :: BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Redis.Redis (Redis.TxResult ())
registerRedis now email hash = Redis.multiExec $ do
  _ <- Redis.sadd "users" [email]
  _ <- Redis.hset "hashes" hash email
  _ <- Redis.hset (userMetadataKey email) "hash" hash
  _ <- Redis.hset (userMetadataKey email) "created_on" now
  _ <- Redis.hset (userMetadataKey email) "accessed_on" now
  return $ pure ()

-- hashes: hgetall from hashes and filter out email
_deleteRedis :: BSU.ByteString -> [BSU.ByteString] -> Redis.Redis (Redis.TxResult ())
_deleteRedis email hashes = Redis.multiExec $ do
  _ <- Redis.hdel "hashes" hashes
  _ <- Redis.srem "users" [email]
  _ <- Redis.del [(userMetadataKey email)]
  return $ pure ()

----

encodeBase64BS :: BS.ByteString -> Text.Text
encodeBase64BS bs = Base64.extractBase64 $ Base64BS.encodeBase64 bs

decodeBase64Text :: Text.Text -> Either Text.Text BS.ByteString
decodeBase64Text txt = Base64BS.decodeBase64Untyped $ TextEnc.encodeUtf8 txt

generateNonce :: IO BS.ByteString
generateNonce = BS.pack <$> Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))

generateAuthHash :: POSIXTime.POSIXTime -> BS.ByteString -> Text.Text -> Hash
generateAuthHash now nonce email = Text.pack $ SHA.showDigest hash2
  where
    now' = BSL.fromStrict $ BSU.fromString $ show (floor now :: Int)
    nonce' = BSL.fromStrict nonce
    email' = BSL.fromStrict $ TextEnc.encodeUtf8 email
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    hash1 = SHA.sha512 $ BSL.concat [ nonce' , email' , now', secret ]
    hash1' = BSL.fromStrict $ BSU.fromString $ SHA.showDigest hash1
    hash2 = SHA.hmacSha512 secret hash1'

generateToken :: POSIXTime.POSIXTime -> TokenState -> Token
generateToken now state = Text.intercalate "." encodedToken
  where
    tokenData = BSL.concat (map BSL.fromStrict state)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    signature = SHA.showDigest $ SHA.hmacSha256 secret tokenData
    expiry = show (floor $ now + 60 * 5 :: Int)
    token = state ++ (map BSU.fromString [expiry,  signature])
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
  if signature == BSU.fromString expectedSignature
    then Right ()
    else Left "Invalid signature"
  where
    tokenData = BSL.concat (map BSL.fromStrict expectedState)
    secret = BSL.fromStrict $ TextEnc.encodeUtf8 "secret"
    expectedSignature = SHA.showDigest $ SHA.hmacSha256 secret tokenData

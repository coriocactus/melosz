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

import Runner
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
    handleGetLogin :: Handler AuthToken
    handleGetLogin = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["123", "456", "789"]
      run $ Right $ AuthToken { authInitToken = token }

    handleGetRegister :: Handler AuthToken
    handleGetRegister = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = generateToken now ["987", "654", "321"]
      run $ Right $ AuthToken { authInitToken = token }

    handlePostLogin :: AuthPayload -> Handler NoContent
    handlePostLogin payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["123", "456", "789"]
      case validateToken now (authPayloadToken payload) tokenState of
        Right () -> do
          let email = TextEnc.encodeUtf8 $ authEmail payload
          eitherEmailExists <- runRedis pool $ Redis.sismember "users" email
          case eitherEmailExists of
            Right True -> do
              let token = generateToken now [ "123", "234", "345", email ]
              _ <- runRedis pool $ Redis.hset "tokens" (TextEnc.encodeUtf8 token) email
              MonadIO.liftIO $ putStrLn $ "Login confirmation: " ++ show ("http://localhost:8080/auth/" <> token)
              run $ Right NoContent
            Right False -> do
              MonadIO.liftIO $ putStrLn $ "User not found: " ++ show email
              run $ Left (err409 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("User not found" :: Text.Text)]) })
            _ -> do
              MonadIO.liftIO $ putStrLn $ "Error: Redis operation failed"
              run $ Left (err500 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Redis operation failed" :: Text.Text)]) })
        Left err -> do
          MonadIO.liftIO $ putStrLn $ "Error: " ++ show err
          run $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= err]) })

    handlePostRegister :: AuthPayload -> Handler NoContent
    handlePostRegister payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = ["987", "654", "321"]
      case validateToken now (authPayloadToken payload) tokenState of
        Right () -> do
          let email = TextEnc.encodeUtf8 $ authEmail payload
          eitherEmailExists <- runRedis pool $ Redis.sismember "users" email
          case eitherEmailExists of
            Right True -> do
              MonadIO.liftIO $ putStrLn $ "Email already registered: " ++ show email
              run $ Left (err409 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Email already registered" :: Text.Text)]) })
            Right False -> do
              let token = generateToken now [ "123", "234", "345", email ]
              _ <- runRedis pool $ Redis.hset "tokens" (TextEnc.encodeUtf8 token) email
              MonadIO.liftIO $ putStrLn $ "Registration confirmation: " ++ show ("http://localhost:8080/auth/" <> token)
              run $ Right NoContent
            _ -> do
              MonadIO.liftIO $ putStrLn $ "Error: Redis operation failed"
              run $ Left (err500 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Redis operation failed" :: Text.Text)]) })
        Left err -> do
          MonadIO.liftIO $ putStrLn $ "Error: " ++ show err
          run $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= err]) })

    handleGetAuth :: Token -> Handler AuthHash
    handleGetAuth token = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      maybeEmail <- runRedis pool $ Redis.hget "tokens" (TextEnc.encodeUtf8 token)
      case maybeEmail of
        Right (Just email) -> do
          _ <- runRedis pool $ Redis.hdel "tokens" [TextEnc.encodeUtf8 token]
          let tokenState = ["123", "234", "345", email]
          case validateToken now token tokenState of
            Right () -> do
              nonce <- BS.pack <$> Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))
              let hash = generateAuthHash now nonce $ TextEnc.decodeUtf8 email
              eitherEmailExists <- runRedis pool $ Redis.sismember "users" email
              case eitherEmailExists of
                Right True -> do
                  MonadIO.liftIO $ putStrLn $ "Login: " ++ show email
                  _ <- runRedis pool $ Redis.multiExec $ do
                    _ <- Redis.hset "hashes" (TextEnc.encodeUtf8 hash) email
                    _ <- Redis.hset email "hash" (TextEnc.encodeUtf8 hash)
                    _ <- Redis.hset email "accessed_on" (BSU.fromString $ show now)
                    return $ pure ()
                  run $ Right $ AuthHash { authHash = hash }
                Right False -> do
                  MonadIO.liftIO $ putStrLn $ "Registration: " ++ show email
                  _ <- runRedis pool $ Redis.multiExec $ do
                    _ <- Redis.sadd "users" [email]
                    _ <- Redis.hset "hashes" (TextEnc.encodeUtf8 hash) email
                    _ <- Redis.hset email "hash" (TextEnc.encodeUtf8 hash)
                    _ <- Redis.hset email "created_on" (BSU.fromString $ show now)
                    _ <- Redis.hset email "accessed_on" (BSU.fromString $ show now)
                    return $ pure ()
                  run $ Right $ AuthHash { authHash = hash }
                _ -> do
                  MonadIO.liftIO $ putStrLn $ "Error: Redis operation failed"
                  run $ Left (err500 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Redis operation failed" :: Text.Text)]) })
            Left err -> do
              MonadIO.liftIO $ putStrLn $ "Error: " ++ show err
              run $ Left (err401 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= err]) })
        Right Nothing -> do
          MonadIO.liftIO $ putStrLn $ "Error: Login link expired"
          run $ Left (err410 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Login link expired" :: Text.Text)]) })
        _ -> do
          MonadIO.liftIO $ putStrLn $ "Error: Redis operation failed"
          run $ Left (err500 { errBody = Aeson.encode (Aeson.object ["error" Aeson..= ("Redis operation failed" :: Text.Text)]) })

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

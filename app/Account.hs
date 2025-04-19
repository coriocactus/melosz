{-# LANGUAGE OverloadedStrings #-}

module Account where

-- import qualified Control.Monad.Reader as MonadReader
-- import qualified Data.ByteString.Char8 as BSC
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Maybe as Maybe
-- import qualified Data.Set as Set
-- import qualified Text.Printf as Printf
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.FormUrlEncoded as Form

import Servant

import Types
import AppState

import Redis
import Actions
import Auth
import Templates

type AccountGetAPI = "account" :> Get '[ServantBlaze.HTML] H.Html

type LogoutGetAPI = "logout" :> Get '[ServantBlaze.HTML] H.Html
type LogoutGetConfirmAPI = "logout" :> "confirm" :> Capture "token" Token :> Get '[ServantBlaze.HTML] H.Html
type LogoutPostConfirmAPI = "logout" :> "confirm" :> ReqBody '[FormUrlEncoded] ConfirmPayload :> Post '[ServantBlaze.HTML] H.Html

type DeleteGetAPI = "delete" :> Get '[ServantBlaze.HTML] H.Html
type DeleteGetConfirmAPI = "delete" :> "confirm" :> Capture "token" Token :> Get '[ServantBlaze.HTML] H.Html
type DeletePostConfirmAPI = "delete" :> "confirm" :> ReqBody '[FormUrlEncoded] ConfirmPayload :> Post '[ServantBlaze.HTML] H.Html

type AccountAPI = EmptyAPI
  :<|> AccountGetAPI
  :<|> LogoutGetAPI
  :<|> LogoutGetConfirmAPI
  :<|> LogoutPostConfirmAPI
  :<|> DeleteGetAPI
  :<|> DeleteGetConfirmAPI
  :<|> DeletePostConfirmAPI

data ConfirmPayload = ConfirmPayload
  { confirmPayloadToken :: Token
  } deriving (Show)

instance Form.FromForm ConfirmPayload where
  fromForm form = ConfirmPayload
    <$> Form.parseUnique "token" form

verifyUser :: RedisPool -> Maybe AuthHeader -> Handler UserId
verifyUser pool maybeAuth =
  case maybeAuth of
    Nothing -> handleUnauthorized
    Just auth -> case extractHash auth of
      Nothing -> handleUnauthorized
      Just hash -> do
        maybeUserId <- getUserByHash pool hash
        case maybeUserId of
          Nothing -> handleUnauthorized
          Just (isRegistered, uid) -> do
            case isRegistered of
              False -> handleUnauthorized
              True -> pure uid

accountServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server AccountAPI
accountServant _cfg pool auth = emptyServer
  :<|> handleGetAccount
  :<|> handleGetLogout
  :<|> handleGetConfirmLogout
  :<|> handlePostConfirmLogout
  :<|> handleGetDelete
  :<|> handleGetConfirmDelete
  :<|> handlePostConfirmDelete
  where
    handleGetAccount :: Handler H.Html
    handleGetAccount = do
      (uid, isRegistered) <- initUser pool auth "/account"
      pure $ mkAccountPage uid isRegistered

    handleGetLogout :: Handler H.Html
    handleGetLogout = do
      UserId email <- verifyUser pool auth
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = encodeText $ generateToken now [ "555", "555", "555", email ]
      _ <- execRedis pool $ setTokenRedis token email

      -- TODO SEND EMAIL
      MonadIO.liftIO $ putStrLn $ "Validate logout request: " ++ show ("http://localhost:5002/logout/confirm/" <> token)
      pure $ emailSentMessage

    handleGetConfirmLogout :: Token -> Handler H.Html
    handleGetConfirmLogout token = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      maybeEmail <- execRedis pool $ getTokenRedis (encodeText token)
      case maybeEmail of
        Left err -> handleRedisError err
        Right Nothing -> handleTokenNotFound
        Right (Just email) -> do
          _ <- execRedis pool $ delTokenRedis (encodeText token)
          let tokenState = ["555", "555", "555", email]

          case validateToken now token tokenState of
            Left err -> handleValidationError err
            Right () -> do
              let newToken = generateToken now [ "5555", "5555", "5555" ]
              _ <- execRedis pool $ setTokenRedis (TextEnc.encodeUtf8 newToken) email

              pure $ mkConfirmPage Logout newToken

    handlePostConfirmLogout :: ConfirmPayload -> Handler H.Html
    handlePostConfirmLogout payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = [ "5555", "5555", "5555" ]

      case validateToken now (confirmPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          maybeEmail <- execRedis pool $ getTokenRedis (encodeText $ confirmPayloadToken payload)
          case maybeEmail of 
            Left err -> handleRedisError err
            Right Nothing -> handleTokenNotFound
            Right (Just email) -> do
              _ <- execRedis pool $ logoutRedis email
              pure $ logoutSuccessfulMessage

    handleGetDelete :: Handler H.Html
    handleGetDelete = do
      UserId email <- verifyUser pool auth
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let token = encodeText $ generateToken now [ "666", "666", "666", email ]
      _ <- execRedis pool $ setTokenRedis token email

      -- TODO SEND EMAIL
      MonadIO.liftIO $ putStrLn $ "Validate delete request: " ++ show ("http://localhost:5002/delete/confirm/" <> token)
      pure $ emailSentMessage

    handleGetConfirmDelete :: Token -> Handler H.Html
    handleGetConfirmDelete token = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      maybeEmail <- execRedis pool $ getTokenRedis (encodeText token)
      case maybeEmail of
        Left err -> handleRedisError err
        Right Nothing -> handleTokenNotFound
        Right (Just email) -> do
          _ <- execRedis pool $ delTokenRedis (encodeText token)
          let tokenState = ["666", "666", "666", email]

          case validateToken now token tokenState of
            Left err -> handleValidationError err
            Right () -> do
              let newToken = generateToken now [ "6666", "6666", "6666" ]
              _ <- execRedis pool $ setTokenRedis (TextEnc.encodeUtf8 newToken) email

              pure $ mkConfirmPage Delete newToken

    handlePostConfirmDelete :: ConfirmPayload -> Handler H.Html
    handlePostConfirmDelete payload = do
      now <- MonadIO.liftIO POSIXTime.getPOSIXTime
      let tokenState = [ "6666", "6666", "6666" ]

      case validateToken now (confirmPayloadToken payload) tokenState of
        Left err -> handleValidationError err
        Right () -> do
          maybeEmail <- execRedis pool $ getTokenRedis (encodeText $ confirmPayloadToken payload)
          case maybeEmail of 
            Left err -> handleRedisError err
            Right Nothing -> handleTokenNotFound
            Right (Just email) -> do
              _ <- execRedis pool $ deleteRedis email
              pure $ deleteSuccessfulMessage

mkAccountPage :: UserId -> Bool -> H.Html
mkAccountPage _uid isRegistered =
  pageLayout (if isRegistered then User else Guest) "melosz" $ do
    H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16" $ do
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[80dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "personal"
      H.div H.! A.class_ "grid grid-cols-2 lg:grid-cols-1 gap-3 content-start" $ do
        H.a H.! A.href "/logout" H.! A.class_ "ds-skeleton h-[20dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "logout"
        H.a H.! A.href "/delete" H.! A.class_ "ds-skeleton h-[20dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "delete"

data Confirm = Logout | Delete deriving (Eq, Show)

mkConfirmPage :: Confirm -> Token -> H.Html
mkConfirmPage confirmAction token = H.docTypeHtml $ H.html $ do
  pageHead title mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.h1 H.! A.class_ "text-2xl font-bold mb-4" $ H.toHtml title
      H.p H.! A.class_ "mb-8 text-center max-w-md" $ H.toHtml (warningMsg :: Text.Text)
      H.form H.! A.method "POST" H.! A.action (H.textValue actionUrl) H.! A.class_ "mb-8" $ do
        H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
        H.button H.! A.type_ "submit" H.! btnClass $ H.toHtml (actionBtnText :: Text.Text)
      H.a H.! A.href (H.textValue cancelRedirect) H.! A.class_ "px-8 py-4 ds-btn ds-btn-secondary" $ "cancel"
  where
    (title, warningMsg, actionUrl, actionBtnText, cancelRedirect, btnClass) = case confirmAction of
      Logout ->
        ( "confirm logout"
        , "are you sure you want to logout from all devices?"
        , "/logout/confirm"
        , "logout"
        , "/account"
        , A.class_ "px-8 py-4 ds-btn ds-btn-warning"
        )
      Delete ->
        ( "confirm account delete"
        , "warning: this action cannot be undone. all your data will be permanently deleted."
        , "/delete/confirm"
        , "delete account"
        , "/account"
        , A.class_ "px-8 py-4 ds-btn ds-btn-error"
        )

logoutSuccessfulMessage :: H.Html
logoutSuccessfulMessage = fastMessage "logout successful" ("/", "home")

deleteSuccessfulMessage :: H.Html
deleteSuccessfulMessage = fastMessage "account deleted" ("/", "home")

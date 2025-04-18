{-# LANGUAGE OverloadedStrings #-}

module Home where

-- import qualified Data.ByteString.Char8 as BSC
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Maybe as Maybe
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as TextEnc
-- import qualified Text.Blaze.Html.Renderer.Utf8 as R
-- import qualified Control.Monad.IO.Class as MonadIO
-- import qualified Control.Monad.Reader as MonadReader
-- import qualified Data.Set as Set
-- import qualified Text.Printf as Printf
-- import qualified Web.FormUrlEncoded as Form
import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Servant

import Types
import AppState

import Redis
-- import Actions
import Auth
import Templates

type HomeGetAPI = Get '[ServantBlaze.HTML] H.Html
type AccountGetAPI = "account" :> Get '[ServantBlaze.HTML] H.Html
type HomeAPI = EmptyAPI
  :<|> HomeGetAPI
  :<|> AccountGetAPI

homeServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server HomeAPI
homeServant _cfg pool auth = emptyServer
  :<|> handleGetHome
  :<|> handleGetAccount
  where
    handleGetHome :: Handler H.Html
    handleGetHome = do
      (uid, isRegistered) <- initUser pool auth "/"
      pure $ mkHomePage uid isRegistered

    handleGetAccount :: Handler H.Html
    handleGetAccount = do
      (uid, isRegistered) <- initUser pool auth "/account"
      pure $ mkAccountPage uid isRegistered

mkHomePage :: UserId -> Bool -> H.Html
mkHomePage _uid _isRegistered =
  pageLayout "melosz" $ do
    H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16" $ do
      H.a H.! A.href "/compare" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "a/b"
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "democracy"
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "singles"
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "doubles"

mkAccountPage :: UserId -> Bool -> H.Html
mkAccountPage _uid _isRegistered =
  pageLayout "melosz" $ do
    H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16" $ do
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[80dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "personal"
      H.div H.! A.class_ "grid grid-cols-2 lg:grid-cols-1 gap-3 content-start" $ do
        H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[20dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "logout"
        H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[20dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ "delete"

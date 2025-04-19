{-# LANGUAGE OverloadedStrings #-}

module Home where

import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Servant

import Types
import AppState

import Redis
import Auth
import Templates

type HomeGetAPI = Get '[ServantBlaze.HTML] H.Html

type HomeAPI = EmptyAPI
  :<|> HomeGetAPI

homeServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server HomeAPI
homeServant _cfg pool auth = emptyServer
  :<|> handleGetHome
  where
    handleGetHome :: Handler H.Html
    handleGetHome = do
      (uid, isRegistered) <- initUser pool auth "/"
      pure $ mkHomePage uid isRegistered

mkHomePage :: UserId -> Bool -> H.Html
mkHomePage _uid isRegistered =
  pageLayout (if isRegistered then User else Guest) "melosz" $ do
    H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-2 gap-8 lg:gap-16" $ do
      H.a H.! A.href "/megusta" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "megusta"
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "realtime"
      H.a H.! A.href "/planck" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "planck"
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-96 justify-self-stretch transition-transform hover:scale-101 duration-300" $ "doubles"

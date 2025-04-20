{-# LANGUAGE OverloadedStrings #-}

module Planck where

import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Servant

import Types
import AppState

import Redis
import Auth
import Templates

type PlanckGetAPI = "planck" :> Get '[ServantBlaze.HTML] H.Html

type PlanckAPI = EmptyAPI
  :<|> PlanckGetAPI

planckServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server PlanckAPI
planckServant _cfg pool auth = emptyServer
  :<|> handleGetPlanck
  where
    handleGetPlanck :: Handler H.Html
    handleGetPlanck = do
      (uid, isRegistered) <- initUser pool auth "/"
      case isRegistered of
        True -> pure $ mkPlanckSpace uid
        False -> handleUnauthorized

mkPlanckSpace :: UserId -> H.Html
mkPlanckSpace _uid =
  pageLayout User "planck space" $ do
    H.div H.! A.class_ "grid grid-cols-1 lg:grid-cols-2 gap-4 lg:gap-8" $ do
      H.div H.! A.class_ "mt-auto mb-auto" $
        H.form H.! A.method "POST" H.! A.action "/planck" H.! A.class_ "flex flex-col gap-2" $ do
          H.input H.! A.class_ "ds-input ds-input-primary ds-validator w-full" H.! A.type_ "email" H.! A.name "email" H.! A.placeholder "confirm email to enter planck space" H.! A.autocomplete "email"
          H.button H.! A.class_ "ds-btn ds-btn-primary" H.! A.type_ "submit" $ "submit"
      H.div H.! A.class_ "h-[40dvh] bg-red-50 justify-self-stretch transition-transform hover:scale-101 duration-300 rounded-lg" $ ""
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[40dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ ""
      H.a H.! A.href "#" H.! A.class_ "ds-skeleton h-[40dvh] justify-self-stretch transition-transform hover:scale-101 duration-300" $ ""

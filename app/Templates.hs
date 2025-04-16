{-# LANGUAGE OverloadedStrings #-}

module Templates where

import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import Servant

pageHead :: Text.Text -> H.Html -> H.Html
pageHead title more = H.head $ do
  H.title $ H.toHtml title
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
  H.meta H.! A.charset "utf-8"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  H.link H.! A.rel "stylesheet" H.! A.href "/styles/output.css"
  more

meloszTitle :: H.Html
meloszTitle = H.div "melosz"

pageLayout :: Text.Text -> H.Html -> H.Html
pageLayout title bodyContent = H.docTypeHtml $ do
  pageHead title mempty
  H.body $ H.div H.! A.class_ "container" $ do
    H.h1 $ H.toHtml title
    bodyContent

data MessageTemplate = MessageTemplate
  { messageTitle :: Text.Text
  , messageHeading :: Text.Text
  , messageLink :: (Text.Text, Text.Text)
  }

mkMessageTemplate :: MessageTemplate -> H.Html
mkMessageTemplate template = H.docTypeHtml $ H.html $ do
  pageHead (messageTitle template) mempty
  H.body $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center" $ do
      H.div H.! A.class_ "md:text-3xl text-xl" $ H.a H.! A.href "/" $ meloszTitle
      H.h1 H.! A.class_ "text-2xl font-bold mt-4 mb-4" $ H.toHtml $ messageHeading template
      H.div H.! A.class_ "mb-32" $ do
        H.a H.! A.class_ "ds-link ds-link-primary ds-link-hover" H.! A.href (H.textValue $ fst $ messageLink template) $
          H.toHtml $ snd $ messageLink template

fastTemplate :: Text.Text -> (Text.Text, Text.Text) -> H.Html
fastTemplate message link = mkMessageTemplate MessageTemplate
  { messageTitle = message
  , messageHeading = message
  , messageLink = link
  }

errorFormatter :: ErrorFormatter
errorFormatter _typeRep _req errMsg = err400 
  { errHeaders = [("Content-Type", "text/html; charset=utf-8")] 
  , errBody = R.renderHtml $ mkMessageTemplate template
  }
  where 
    template = MessageTemplate
      { messageTitle = "error"
      , messageHeading = Text.pack errMsg
      , messageLink = ("/", "home")
      }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter _req = err404
  { errHeaders = [("Content-Type", "text/html; charset=utf-8")] 
  , errBody = R.renderHtml $ mkMessageTemplate template
  }
  where 
    template = MessageTemplate
      { messageTitle = "error"
      , messageHeading = "not found"
      , messageLink = ("/", "home")
      }

emailSentTemplate :: H.Html
emailSentTemplate = mkMessageTemplate template
  where
    template = MessageTemplate
      { messageTitle = "check your email"
      , messageHeading = "check your email"
      , messageLink = ("/", "home")
      }

authenticatedTemplate :: H.Html
authenticatedTemplate = mkMessageTemplate template
  where
    template = MessageTemplate
      { messageTitle = "success"
      , messageHeading = "authenticated"
      , messageLink = ("/", "home")
      }

loginTemplate :: Text.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  pageHead "login" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "md:text-3xl text-xl mb-8" $ H.a H.! A.href "/" $ meloszTitle
      H.div H.! A.class_ "w-full max-w-sm mx-auto mb-8" $ do
        H.form H.! A.class_ "flex flex-col" H.! A.method "POST" H.! A.action "/login" $ do
          H.input H.! A.type_ "email" H.! A.name "email" H.! A.placeholder "email" H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300" H.! A.required "required"
          H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $ "login"
      H.a H.! A.class_ "ds-link ds-link-primary ds-link-hover" H.! A.href "/register" $ "register"

registerTemplate :: Text.Text -> H.Html
registerTemplate token = H.docTypeHtml $ H.html $ do
  pageHead "register" mempty
  H.body H.! A.class_ "min-h-screen bg-base-100 text-base-content" $ do
    H.div H.! A.class_ "min-h-screen flex flex-col items-center justify-center p-4" $ do
      H.div H.! A.class_ "md:text-3xl text-xl mb-8" $ H.a H.! A.href "/" $ meloszTitle
      H.div H.! A.class_ "w-full max-w-sm mx-auto mb-8" $ do
        H.form H.! A.class_ "flex flex-col" H.! A.method "POST" H.! A.action "/register" $ do
          H.input H.! A.type_ "email" H.! A.name "email" H.! A.placeholder "email" H.! A.class_ "p-2 font-inherit text-inherit rounded-lg border border-base-300" H.! A.required "required"
          H.input H.! A.type_ "hidden" H.! A.name "token" H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "w-full p-2 mt-4 font-inherit bg-primary text-primary-content rounded-lg cursor-pointer hover:bg-primary/90" $ "register"
      H.a H.! A.class_ "ds-link ds-link-primary ds-link-hover" H.! A.href "/login" $ "login"

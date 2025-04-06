{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Internal as I

import qualified Servant as Servant
import qualified Servant.HTML.Blaze as ServantBlaze
import Servant (Get)
import Servant (PlainText)
import Servant ((:>), (:<|>)(..))

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | APPLICATION

application :: Wai.Application
application = notFoundMiddleware servant

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | MIDDLEWARE

notFoundMiddleware :: Wai.Middleware
notFoundMiddleware app req respond = app req $ \response ->
  case Wai.responseStatus response of
    status | status == HTTP.status404 -> do
      MonadIO.liftIO $ putStrLn $ "Not Found: " ++ show (Wai.rawPathInfo req)
      respond $ Wai.responseLBS
        HTTP.status404
        [("Content-Type", "text/plain")]
        "Not Found: The requested resource does not exist."
    _ -> respond response

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANTS

type API =
  HomeAPI :<|>
  GreetAPI :<|>
  Servant.EmptyAPI

servant :: Servant.Application
servant = Servant.serve translator servants
  where
    translator :: Servant.Proxy API
    translator = Servant.Proxy

    servants :: Servant.Server API
    servants =
      homeServant :<|>
      greetServant :<|>
      Servant.emptyServer

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: HOME

type HomeAPI =
  Get '[ServantBlaze.HTML] H.Html

homeServant :: Servant.Server HomeAPI
homeServant =
  handleHome
  where
    handleHome :: Servant.Handler H.Html
    handleHome = do
      MonadIO.liftIO $ putStrLn "Received request for /"
      return $ mkHome

    mkHome :: H.Html
    mkHome =
      H.docTypeHtml $ pageHead "Home" $ H.body $ do
        H.h1 "HOME!"
        H.a H.! A.href "/hello" $ "Say Hello"
        H.br
        H.a H.! A.href "/bye" $ "Say Goodbye"

pageHead :: Text.Text -> H.Html -> H.Html
pageHead title more = H.head $ do
  H.title $ H.toHtml title
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
  H.meta H.! A.charset "utf-8"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  -- H.link H.! A.rel "stylesheet" H.! A.href "/styles/output.css"
  more

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: GREET

type GreetAPI =
  "hello" :> Get '[PlainText] Text.Text :<|>
  "bye"   :> Get '[PlainText] Text.Text

greetServant :: Servant.Server GreetAPI
greetServant =
  handleHello :<|>
  handleBye
  where
    handleHello :: Servant.Handler Text.Text
    handleHello = do
      MonadIO.liftIO $ putStrLn "Received request for /hello"
      return "Hello from Servant!"

    handleBye :: Servant.Handler Text.Text
    handleBye = do
      MonadIO.liftIO $ putStrLn "Received request for /bye"
      return "Goodbye from Servant!"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Text.Printf as Printf
import qualified Web.FormUrlEncoded as Form

import qualified Servant as Servant
import qualified Servant.HTML.Blaze as ServantBlaze
import Servant (Get, PostNoContent, ReqBody, FormUrlEncoded)
import Servant ((:>), (:<|>)(..))

import Types
import AppState
import Marshal
import Rating
import Scheduler

-- application

main :: IO ()
main = runWeb 8080

runWeb :: Int -> IO ()
runWeb port = do
  let appToRun :: App AppConfig
      appToRun = setupColourfulUser >> MonadReader.ask
  putStrLn $ "Starting server on port " ++ show port
  runner port appToRun

runner :: Int -> App AppConfig -> IO ()
runner port app = do
  let initialOptions = colourfulOptions
      user = "coriocactus"

  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let initialConfig = AppConfig
        { configSystemTau = 0.5
        , configStateRef = initialStateRef
        , configOptions = initialOptions
        }

  finalConfig <- MonadReader.runReaderT app initialConfig
  Warp.run port (application finalConfig)


colourfulOptions :: Set.Set Option
colourfulOptions = Set.fromList
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

setupColourfulUser :: App ()
setupColourfulUser = do
  let user = "coriocactus"
  setupUser user

-- webserver

application :: AppConfig -> Wai.Application
application cfg = notFoundMiddleware (Servant.serve butler (servants cfg))

type API = Servant.EmptyAPI
  :<|> HomeAPI
  :<|> CompareAPI

butler :: Servant.Proxy API
butler = Servant.Proxy

servants :: AppConfig -> Servant.Server API
servants cfg = Servant.emptyServer
  :<|> homeServant cfg
  :<|> compareServant cfg

notFoundMiddleware :: Wai.Middleware
notFoundMiddleware app req respond = app req $ \response ->
  case Wai.responseStatus response of
    status | status == HTTP.status404 -> do
      MonadIO.liftIO $ putStrLn $ "Not Found: " ++ show (Wai.rawPathInfo req) ++ " (Responding with 404 page)"
      let body = R.renderHtml $ mkErrorPage "The requested resource was not found."
      respond $ Wai.responseLBS
        HTTP.status404
        [("Content-Type", "text/html; charset=utf-8")]
        body
    _ -> respond response

runApp :: AppConfig -> App a -> Servant.Handler a
runApp cfg action = MonadIO.liftIO $ MonadReader.runReaderT action cfg

-- servant

type HomeAPI = Servant.EmptyAPI
  :<|> Get '[ServantBlaze.HTML] H.Html

homeButler :: Servant.Proxy HomeAPI
homeButler = Servant.Proxy

homeServant :: AppConfig -> Servant.Server HomeAPI
homeServant _cfg = Servant.emptyServer
  :<|> handleHome
  where
    handleHome :: Servant.Handler H.Html
    handleHome = do
      MonadIO.liftIO $ putStrLn "Serving /"
      let userUrl = Servant.safeLink butler compareGetButler (Text.pack "coriocactus")
      throwRedirect userUrl

throwRedirect :: Servant.Link -> Servant.Handler a
throwRedirect link =
  Servant.throwError Servant.err302 { Servant.errHeaders = [("Location", location)] }
  where
    location = TextEnc.encodeUtf8 $ Text.pack $ "/" ++ (Servant.uriPath $ Servant.linkURI link)

data HtmlComparisonStatus = HtmlComparisonStatus
  { htmlStatusConsistency :: Double
  , htmlStatusIsComplete :: Bool
  }

data ChooseFormData = ChooseFormData
  { formWinnerId :: Text.Text
  , formLoserId  :: Text.Text
  } deriving (Show)

instance Form.FromForm ChooseFormData where
  fromForm form = ChooseFormData
    <$> Form.parseUnique "winnerId" form
    <*> Form.parseUnique "loserId" form

type CompareGetAPI = "compare" :> Servant.Capture "userid" Text.Text :> Get '[ServantBlaze.HTML] H.Html
type ComparePostAPI = "compare" :> Servant.Capture "userid" Text.Text :> ReqBody '[FormUrlEncoded] ChooseFormData :> PostNoContent

type CompareAPI = Servant.EmptyAPI
  :<|> CompareGetAPI
  :<|> ComparePostAPI

compareGetButler :: Servant.Proxy CompareGetAPI
compareGetButler = Servant.Proxy

compareServant :: AppConfig -> Servant.Server CompareAPI
compareServant cfg = Servant.emptyServer
  :<|> handleGetCompare
  :<|> handlePostCompare
  where
    fetchAndRenderComparePage :: UserId -> App H.Html
    fetchAndRenderComparePage userId = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show userId
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      currentRatings <- getUserRatings userId optionsList
      mPair <- getNextComparisonPair userId
      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show userId ++ ": " ++ show mPair
      pure $ mkComparePage userId mPair currentRatings

    handleGetCompare :: Text.Text -> Servant.Handler H.Html
    handleGetCompare userIdText = do
      MonadIO.liftIO $ putStrLn $ "Serving /compare/" ++ Text.unpack userIdText
      let userId = UserId (TextEnc.encodeUtf8 userIdText)
      runApp cfg $ do
        mUserState <- getUserState userId
        case mUserState of
          Nothing -> do
            MonadIO.liftIO $ putStrLn ("User not found, creating: " ++ show userId)
            setupUser userId
            fetchAndRenderComparePage userId
          Just _ -> do
            fetchAndRenderComparePage userId

    handlePostCompare :: Text.Text -> ChooseFormData -> Servant.Handler Servant.NoContent
    handlePostCompare userIdText formData = do
      MonadIO.liftIO $ putStrLn $ "Processing POST /compare/" ++ Text.unpack userIdText ++ " with data: " ++ show formData

      let userId   = UserId (TextEnc.encodeUtf8 userIdText)
          winnerId = OptionId (TextEnc.encodeUtf8 $ formWinnerId formData)
          loserId  = OptionId (TextEnc.encodeUtf8 $ formLoserId formData)

      runApp cfg $ do
        mWinnerOpt <- getOptionById winnerId
        mLoserOpt  <- getOptionById loserId

        case (mWinnerOpt, mLoserOpt) of
          (Just winnerOpt, Just loserOpt) -> do
            MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show userId ++ ": " ++ show (optionName winnerOpt) ++ " (Win) vs " ++ show (optionName loserOpt)
            updateRatings userId winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show userId
          _ -> MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"

      let redirectLink = Servant.safeLink butler compareGetButler userIdText
          redirectUrlPath = Text.pack $ "/" ++ Servant.uriPath (Servant.linkURI redirectLink)

      MonadIO.liftIO $ putStrLn $ "Redirecting to: " ++ Text.unpack redirectUrlPath
      Servant.throwError Servant.err303 { Servant.errHeaders = [("Location", TextEnc.encodeUtf8 redirectUrlPath)] }

mkComparePage :: UserId -> Maybe (Option, Option) -> [(Option, Double)] -> H.Html
mkComparePage userId mPair rankings =
  pageLayout ("Comparison for " <> TextEnc.decodeUtf8 (unUserId userId)) $ do
    case mPair of
      Just (opt1, opt2) -> mkComparisonSection userId opt1 opt2
      Nothing -> H.div H.! A.class_ "completion-message" $ "No more pairs to compare based on current strategy."

    H.div H.! A.class_ "results-grid" $ do
      H.div H.! A.class_ "rankings-section" $ do
        H.h2 "Current Rankings"
        mkRankingsTable rankings

mkComparisonSection :: UserId -> Option -> Option -> H.Html
mkComparisonSection userId opt1 opt2 = do
  let uidText = TextEnc.decodeUtf8 (unUserId userId)
      oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
      oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)
      postUrl = H.textValue $ Text.pack $ "/compare/" ++ Text.unpack uidText

  H.div H.! A.class_ "comparison-box" $ do
    H.h3 "Which do you prefer?" H.! A.style "width: 100%; text-align: center; margin-bottom: 1em;"
    H.div H.! A.style "display: flex; justify-content: space-around; width: 100%;" $ do
      H.form H.! A.method "post" H.! A.action postUrl H.! A.class_ "option-form" $ do
        H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid1Text)
        H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid2Text)
        H.button H.! A.type_ "submit" H.! A.class_ "option-button" $
          H.toHtml (BSC.unpack $ optionName opt1)

      H.span H.! A.style "align-self: center; font-weight: bold;" $ " OR "

      H.form H.! A.method "post" H.! A.action postUrl H.! A.class_ "option-form" $ do
        H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid2Text)
        H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid1Text)
        H.button H.! A.type_ "submit" H.! A.class_ "option-button" $
          H.toHtml (BSC.unpack $ optionName opt2)

mkRankingsTable :: [(Option, Double)] -> H.Html
mkRankingsTable sortedRatings = H.table $ do
  H.thead $ H.tr $ do
    H.th "Rank"
    H.th "Option"
    H.th "Rating"
  H.tbody $ do
    mapM_ mkRankingRow (zip [1..] sortedRatings)
  where
    mkRankingRow :: (Int, (Option, Double)) -> H.Html
    mkRankingRow (rank, (opt, rating)) = H.tr $ do
      H.td $ H.toHtml rank
      H.td $ H.toHtml (BSC.unpack $ optionName opt)
      H.td $ H.toHtml (Printf.printf "%.1f" rating :: String)

getOptionById :: OptionId -> App (Maybe Option)
getOptionById oidToFind = do
  optionsSet <- MonadReader.asks configOptions
  pure $ List.find (\opt -> optionId opt == oidToFind) (Set.toList optionsSet)

-- blaze templates

pageLayout :: Text.Text -> H.Html -> H.Html
pageLayout title bodyContent = H.docTypeHtml $ do
  pageHead title (H.style styleSheet)
  H.body $ H.div H.! A.class_ "container" $ do
    H.h1 $ H.toHtml title
    bodyContent

pageHead :: Text.Text -> H.Html -> H.Html
pageHead title more = H.head $ do
  H.title $ H.toHtml title
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
  H.meta H.! A.charset "utf-8"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  more

styleSheet :: H.Html
styleSheet = H.toHtml (Text.unlines
  [ "body { font-family: sans-serif; margin: 2em; background-color: #f8f8f8; color: #333; }"
  , ".container { max-width: 800px; margin: auto; padding: 2em; background-color: #fff; border: 1px solid #ccc; border-radius: 8px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); }"
  , "h1 { text-align: center; color: #333; margin-bottom: 1em; }"
  , "h2 { color: #555; border-bottom: 1px solid #eee; padding-bottom: 0.3em; margin-top: 1.5em; }"
  , "h3 { text-align: center; color: #666; margin-bottom: 1em; }"
  , ".comparison-box { border: 1px solid #ddd; padding: 1.5em; margin-bottom: 2em; display: flex; flex-direction: column; align-items: center; background-color: #fdfdfd; border-radius: 5px; }"
  , ".option-form { display: inline-block; margin: 0; }"
  , ".option-button { min-width: 120px; padding: 0.8em 1.5em; text-decoration: none; background-color: #e7e7e7; border: 1px solid #ccc; border-radius: 4px; color: black; font-size: 1.1em; cursor: pointer; transition: background-color 0.2s ease; }"
  , ".option-button:hover { background-color: #d0d0d0; }"
  , ".results-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 2em; margin-top: 2em; }"
  , "table { border-collapse: collapse; width: 100%; margin-bottom: 1em; }"
  , "th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }"
  , "th { background-color: #f2f2f2; font-weight: bold; }"
  , "tr:nth-child(even) { background-color: #f9f9f9; }"
  , "td:first-child { text-align: center; font-weight: bold; }"
  , ".rankings-section { border: 1px solid #ddd; padding: 1.5em; background-color: #fdfdfd; border-radius: 5px; }"
  , ".completion-message { color: #080; font-weight: bold; text-align: center; padding: 2em; background-color: #e8f8e8; border: 1px solid #b8d8b8; border-radius: 5px; margin-top: 2em; }"
  , "a { color: #007bff; text-decoration: none; }"
  , "a:hover { text-decoration: underline; }"
  ])

mkErrorPage :: String -> H.Html
mkErrorPage errorMsg = pageLayout "Error" $ do
  H.h2 "An Error Occurred"
  H.p H.! A.style "color: #c00; font-weight: bold;" $ H.toHtml errorMsg
  H.p $ H.a H.! A.href "/" $ "Go back home"

-- utils

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

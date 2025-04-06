{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Web where

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
-- import qualified Text.Blaze.Internal as I
import qualified Text.Printf as Printf
import qualified Web.FormUrlEncoded as Form

import qualified Servant as Servant
import qualified Servant.HTML.Blaze as ServantBlaze
import Servant (Get, PostNoContent, ReqBody, FormUrlEncoded)
import Servant ((:>), (:<|>)(..))

import Types
import AppState
import Marshal
import Preference
import Rating
import Scheduler

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Web Runner

runner :: Int -> App AppConfig -> IO ()
runner port app = do
  initialStateRef <- MonadIO.liftIO $ IORef.newIORef initialState
  let config = AppConfig
        { configKFactor = 32.0
        , configInitialRating = 1500.0
        , configStateRef = initialStateRef
        }
  initialConfig <- MonadReader.runReaderT app config
  Warp.run port (application initialConfig)

colourfulScaffold :: App AppConfig
colourfulScaffold = do
  let options =
        [ createOption "red" "Red"
        , createOption "orange" "Orange"
        , createOption "yellow" "Yellow"
        , createOption "green" "Green"
        , createOption "blue" "Blue"
        , createOption "violet" "Violet"
        , createOption "indigo" "Indigo"
        , createOption "cyan" "Cyan"
        , createOption "magenta" "Magenta"
        ]
  let user = "coriocactus"

  setupOptions options
  setupUser user

  MonadReader.ask

runWebserver :: Int -> IO ()
runWebserver port = do
  let appToRun :: App AppConfig
      appToRun = colourfulScaffold
  putStrLn $ "Starting server on port " ++ show port
  runner port appToRun

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|

application :: AppConfig -> Wai.Application
application cfg = notFoundMiddleware (Servant.serve butler (servants cfg))

type API = Servant.EmptyAPI
  :<|> HomeAPI
  :<|> CompareAPI
  :<|> StaticAPI

butler :: Servant.Proxy API
butler = Servant.Proxy

servants :: AppConfig -> Servant.Server API
servants cfg = Servant.emptyServer
  :<|> homeServant cfg
  :<|> compareServant cfg
  :<|> staticServant cfg

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

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: HOME

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

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: COMPARE

data ComparisonStatus = ComparisonStatus
  { statusProgress :: (Int, Int, Double)
  , statusAgreement :: Double
  , statusConsistency :: Double
  , statusViolations :: [(Option, Option, Option)]
  , statusIsComplete :: Bool
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
    handleGetCompare :: Text.Text -> Servant.Handler H.Html
    handleGetCompare userIdText = do
      MonadIO.liftIO $ putStrLn $ "Serving /compare/" ++ Text.unpack userIdText
      let userId = UserId (TextEnc.encodeUtf8 userIdText)
      runApp cfg $ do
        mUserState <- getUserState userId
        case mUserState of
          Nothing -> MonadIO.liftIO $ putStrLn ("User not found: " ++ show userId) >> pure (mkErrorPage "User not found")
          Just _ -> do
            MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show userId
            optionsList <- Set.toList <$> getOptions
            currentRatings <- getUserRatings userId optionsList
            violationsSet <- getViolationsForUser userId
            let violationPairs = findPairsInViolations violationsSet

            mPair <- getNextComparisonPair userId currentRatings violationPairs

            statusData <- gatherStatusData userId currentRatings violationsSet

            MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show userId ++ ": " ++ show mPair
            pure $ mkComparePage userId mPair currentRatings statusData

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
            recordComparison userId winnerOpt loserOpt Win
            updateRatings userId winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show userId
          _ -> MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"

      let redirectLink = Servant.safeLink butler compareGetButler userIdText
          redirectUrlPath = Text.pack $ "/" ++ Servant.uriPath (Servant.linkURI redirectLink)

      MonadIO.liftIO $ putStrLn $ "Redirecting to: " ++ Text.unpack redirectUrlPath
      Servant.throwError Servant.err303 { Servant.errHeaders = [("Location", TextEnc.encodeUtf8 redirectUrlPath)] }

mkComparePage :: UserId -> Maybe (Option, Option) -> [(Option, Double)] -> ComparisonStatus -> H.Html
mkComparePage userId mPair rankings status =
  pageLayout ("Comparison for " <> TextEnc.decodeUtf8 (unUserId userId)) $ do
    case mPair of
      Just (opt1, opt2) -> mkComparisonSection userId opt1 opt2
      Nothing ->
        case statusIsComplete status of
          True -> H.div H.! A.class_ "completion-message" $ "Ranking complete and consistent!"
          False -> H.div H.! A.class_ "completion-message" $ "No more pairs to compare based on current strategy."

    H.div H.! A.class_ "results-grid" $ do
      H.div H.! A.class_ "rankings-section" $ do
        H.h2 "Current Rankings"
        mkRankingsTable rankings
      H.div H.! A.class_ "status-section" $ do
        H.h2 "Session Status"
        mkStatusSection status

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
      H.td $ H.toHtml (Printf.printf "%.2f" rating :: String)

mkStatusSection :: ComparisonStatus -> H.Html
mkStatusSection status = do
  let (completed, total, percent) = statusProgress status
      progressStr = Printf.printf "Progress: %d/%d pairs compared (%.1f%%)" completed total percent :: String
      agreementStr = Printf.printf "Agreement (vs ELO): %.2f%%" (statusAgreement status) :: String
      consistencyStr = Printf.printf "Consistency (Transitivity): %.2f%%" (statusConsistency status) :: String

  H.p $ H.toHtml progressStr
  H.p $ H.toHtml agreementStr
  H.p $ H.toHtml consistencyStr

  if null (statusViolations status)
    then H.p "No transitivity violations."
    else do
      let violationsStr = Printf.printf "Detected %d transitivity violation(s):" (length $ statusViolations status) :: String
      H.p $ H.toHtml violationsStr
      H.ul H.! A.class_ "violation-list" $ do
        mapM_ (H.li . H.toHtml . formatViolationHtml) (statusViolations status)

mkComparisonSection :: UserId -> Option -> Option -> H.Html
mkComparisonSection userId opt1 opt2 = do
  let uidText = TextEnc.decodeUtf8 (unUserId userId)
      oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
      oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)
      postUrl = H.textValue $ Text.pack $ "/compare/" ++ Text.unpack uidText

  H.div H.! A.class_ "comparison-box" $ do
    H.form H.! A.method "post" H.! A.action postUrl H.! A.class_ "option-form" $ do
      H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid1Text) -- opt1 wins
      H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid2Text)  -- opt2 loses
      H.button H.! A.type_ "submit" H.! A.class_ "option-button" $
        H.toHtml (BSC.unpack $ optionName opt1)

    H.span H.! A.style "margin: 0 1em;" $ " OR "

    H.form H.! A.method "post" H.! A.action postUrl H.! A.class_ "option-form" $ do
      H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid2Text)  -- opt2 wins
      H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid1Text) -- opt1 loses
      H.button H.! A.type_ "submit" H.! A.class_ "option-button" $
        H.toHtml (BSC.unpack $ optionName opt2)

getOptionById :: OptionId -> App (Maybe Option)
getOptionById oidToFind = do
  optionsSet <- getOptions
  pure $ List.find (\opt -> optionId opt == oidToFind) (Set.toList optionsSet)

formatViolationHtml :: (Option, Option, Option) -> String
formatViolationHtml (a, c, b) =
  Printf.printf "%s > %s, %s > %s, but %s > %s"
    (BSC.unpack $ optionName a) (BSC.unpack $ optionName b)
    (BSC.unpack $ optionName b) (BSC.unpack $ optionName c)
    (BSC.unpack $ optionName c) (BSC.unpack $ optionName a)

gatherStatusData :: UserId -> [(Option, Double)] -> Set.Set (Option, Option, Option) -> App ComparisonStatus
gatherStatusData uid ratings violationsSet = do
  optionsCount <- Set.size <$> getOptions
  uncomparedSet <- getUncomparedPairsForUser uid

  let totalPossiblePairs = if optionsCount < 2 then 0 else optionsCount * (optionsCount - 1) `div` 2
      completedPairs = totalPossiblePairs - Set.size uncomparedSet
      progressPercent = if totalPossiblePairs == 0 then 100.0 else (fromIntegral completedPairs * 100.0 / fromIntegral totalPossiblePairs :: Double)
      violationsList = Set.toList violationsSet

  agreementScore <- calculateAgreementScore uid ratings
  transitivityScore <- calculateTransitivityScore uid
  isComplete <- checkIfComplete uid

  pure $ ComparisonStatus
    { statusProgress = (completedPairs, totalPossiblePairs, progressPercent)
    , statusAgreement = agreementScore * 100.0
    , statusConsistency = transitivityScore * 100.0
    , statusViolations = violationsList
    , statusIsComplete = isComplete && Set.null violationsSet
    }

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: STATIC

type StaticAPI =
  "static" :> Servant.Raw

staticServant :: AppConfig -> Servant.Server StaticAPI
staticServant _cfg =
  Servant.serveDirectoryWebApp "static"

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | BLAZE TEMPLATES

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
  [ "body { font-family: sans-serif; margin: 2em; }"
  , ".container { max-width: 800px; margin: auto; padding: 1em; border: 1px solid #ccc; border-radius: 5px; }"
  , ".comparison-box { border: 1px solid #eee; padding: 1em; margin-bottom: 1em; display: flex; justify-content: center; align-items: center; }"
  , ".option-form { display: inline-block; margin: 0; }"
  , ".option-button { padding: 1em 2em; text-decoration: none; background-color: #eee; border: 1px solid #ccc; border-radius: 4px; color: black; font-size: inherit; cursor: pointer; }"
  , ".option-button:hover { background-color: #ddd; }"
  , ".results-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 2em; margin-top: 2em; }"
  , "table { border-collapse: collapse; width: 100%; }"
  , "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }"
  , "th { background-color: #f2f2f2; }"
  , ".status-section, .rankings-section { border: 1px solid #eee; padding: 1em; }"
  , ".violation-list li { color: red; margin-bottom: 0.5em; }"
  , ".completion-message { color: green; font-weight: bold; text-align: center; padding: 2em; }"
  ])

mkErrorPage :: String -> H.Html
mkErrorPage errorMsg = pageLayout "Error" $ do
  H.h2 "An Error Occurred"
  H.p H.! A.style "color: red;" $ H.toHtml errorMsg
  H.p $ H.a H.! A.href "/" $ "Go back home"

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | UTILS

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

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

import qualified Servant as Servant
import qualified Servant.HTML.Blaze as ServantBlaze
import Servant (Get)
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
application cfg = notFoundMiddleware (Servant.serve translator (servants cfg))

type API = Servant.EmptyAPI
  :<|> HomeAPI
  :<|> CompareAPI
  :<|> ChooseAPI
  :<|> StaticAPI

translator :: Servant.Proxy API
translator = Servant.Proxy

servants :: AppConfig -> Servant.Server API
servants cfg = Servant.emptyServer
  :<|> homeServant cfg
  :<|> compareServant cfg
  :<|> chooseServant cfg
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
runApp cfg appAction = MonadIO.liftIO $ MonadReader.runReaderT appAction cfg

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: HOME

type HomeAPI =
  Get '[ServantBlaze.HTML] H.Html

homeServant :: AppConfig -> Servant.Server HomeAPI
homeServant _cfg = handleHome
  where
    handleHome :: Servant.Handler H.Html
    handleHome = do
      MonadIO.liftIO $ putStrLn "Serving /"
      throwRedirect userUrl
      where 
        userUrl = Servant.safeLink translator (Servant.Proxy :: Servant.Proxy CompareAPI) (Text.pack "coriocactus")

throwRedirect :: Servant.Link -> Servant.Handler a
throwRedirect link =
  Servant.throwError Servant.err302 { Servant.errHeaders = [("Location", location)] }
  where
    location = TextEnc.encodeUtf8 $ Text.pack $ "/" ++ (Servant.uriPath $ Servant.linkURI link)

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: COMPARE

type CompareAPI =
  "compare" :> Servant.Capture "userid" Text.Text :> Get '[ServantBlaze.HTML] H.Html

compareServant :: AppConfig -> Servant.Server CompareAPI
compareServant cfg = handleCompare
  where
    handleCompare :: Text.Text -> Servant.Handler H.Html
    handleCompare userIdText = do
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

data ComparisonStatus = ComparisonStatus
  { statusProgress :: (Int, Int, Double)
  , statusAgreement :: Double
  , statusConsistency :: Double
  , statusViolations :: [(Option, Option, Option)]
  , statusIsComplete :: Bool
  }

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
-- | SERVANT: CHOOSE

type ChooseAPI =
  "choose" :> Servant.Capture "userid" Text.Text :> Servant.Capture "option1id" Text.Text :> Servant.Capture "option2id" Text.Text :> Servant.Capture "choice" Text.Text :> Get '[ServantBlaze.HTML] H.Html

chooseServant :: AppConfig -> Servant.Server ChooseAPI
chooseServant cfg = handleChoose
  where
    handleChoose :: Text.Text -> Text.Text -> Text.Text -> Text.Text -> Servant.Handler H.Html
    handleChoose userIdText oid1Text oid2Text choiceText = do
      MonadIO.liftIO $ putStrLn $ "Processing /choose/" ++ Text.unpack userIdText ++ "/" ++ Text.unpack oid1Text ++ "/" ++ Text.unpack oid2Text ++ "/" ++ Text.unpack choiceText

      let userId = UserId (TextEnc.encodeUtf8 userIdText)
          oid1 = OptionId (TextEnc.encodeUtf8 oid1Text)
          oid2 = OptionId (TextEnc.encodeUtf8 oid2Text)

      let mResult = case Text.toLower choiceText of
            "win1" -> Just Win
            "win2" -> Just Loss
            _      -> Nothing

      case mResult of
        Nothing -> pure $ mkErrorPage "Invalid choice parameter."
        Just result -> do
          runApp cfg $ do
            mOpt1 <- getOptionById oid1
            mOpt2 <- getOptionById oid2

            case (mOpt1, mOpt2) of
              (Just opt1, Just opt2) -> do
                MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show userId ++ ": " ++ show (optionName opt1) ++ " vs " ++ show (optionName opt2) ++ " -> " ++ show result
                recordComparison userId opt1 opt2 result
                updateRatings userId opt1 opt2 result
                MonadIO.liftIO $ putStrLn $ "State updated for " ++ show userId
              _ -> MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show oid1 ++ " or " ++ show oid2 ++ ")"

          let redirectUrl = Servant.safeLink translator (Servant.Proxy :: Servant.Proxy CompareAPI) userIdText
          throwRedirect redirectUrl

getOptionById :: OptionId -> App (Maybe Option)
getOptionById oidToFind = do
  optionsSet <- getOptions
  pure $ List.find (\opt -> optionId opt == oidToFind) (Set.toList optionsSet)

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | SERVANT: STATIC

type StaticAPI = "static" :> Servant.Raw

staticServant :: AppConfig -> Servant.Server StaticAPI
staticServant _cfg = Servant.serveDirectoryWebApp "static"

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | BLAZE

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
  , ".comparison-box { border: 1px solid #eee; padding: 1em; margin-bottom: 1em; display: flex; justify-content: space-around; align-items: center; }"
  , ".option-button { padding: 1em 2em; text-decoration: none; background-color: #eee; border: 1px solid #ccc; border-radius: 4px; color: black; }"
  , ".option-button:hover { background-color: #ddd; }"
  , ".results-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 2em; margin-top: 2em; }"
  , "table { border-collapse: collapse; width: 100%; }"
  , "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }"
  , "th { background-color: #f2f2f2; }"
  , ".status-section, .rankings-section { border: 1px solid #eee; padding: 1em; }"
  , ".violation-list li { color: red; margin-bottom: 0.5em; }"
  , ".completion-message { color: green; font-weight: bold; text-align: center; padding: 2em; }"
  ])

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

mkComparisonSection :: UserId -> Option -> Option -> H.Html
mkComparisonSection userId opt1 opt2 = do
  let uidText = TextEnc.decodeUtf8 (unUserId userId)
      oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
      oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)

      choose1Link = Servant.safeLink translator (Servant.Proxy :: Servant.Proxy ChooseAPI) uidText oid1Text oid2Text (Text.pack "win1")
      choose2Link = Servant.safeLink translator (Servant.Proxy :: Servant.Proxy ChooseAPI) uidText oid1Text oid2Text (Text.pack "win2")

  H.div H.! A.class_ "comparison-box" $ do
    H.a H.! A.href (H.textValue $ Text.pack $ ("/" ++ (show $ Servant.linkURI choose1Link))) H.! A.class_ "option-button" $
      H.toHtml (BSC.unpack $ optionName opt1)
    H.span " OR "
    H.a H.! A.href (H.textValue $ Text.pack $ ("/" ++ (show $ Servant.linkURI choose2Link))) H.! A.class_ "option-button" $
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

mkErrorPage :: String -> H.Html
mkErrorPage errorMsg = pageLayout "Error" $ do
  H.h2 "An Error Occurred"
  H.p H.! A.style "color: red;" $ H.toHtml errorMsg
  H.p $ H.a H.! A.href "/" $ "Go back home"

formatViolationHtml :: (Option, Option, Option) -> String
formatViolationHtml (a, c, b) =
  Printf.printf "%s > %s, %s > %s, but %s > %s"
    (BSC.unpack $ optionName a) (BSC.unpack $ optionName b)
    (BSC.unpack $ optionName b) (BSC.unpack $ optionName c)
    (BSC.unpack $ optionName c) (BSC.unpack $ optionName a)

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

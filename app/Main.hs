{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Servant.HTML.Blaze as ServantBlaze
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Printf as Printf
import qualified Web.FormUrlEncoded as Form

import Servant

import Types
import AppState
import Rating
import Scheduler

import Redis
import Actions
import Templates
import Auth

-- application

main :: IO ()
main = launch 5002

launch :: Int -> IO ()
launch port = do
  let options = colourfulOptions

  pool <- mkRedisPool
  let redisHandle = mkRedisHandle pool options

  let config = AppConfig
        { configOptions = options
        , configSystemTau = 0.5
        , configStateHandle = redisHandle
        }

  putStrLn $ "=== === === running melosz backend === === ==="
  putStrLn $ "listening: http://localhost:" ++ show port

  Warp.run port (application config pool)

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

-- webserver

application :: AppConfig -> RedisPool -> Wai.Application
application cfg pool = Gzip.gzip Gzip.defaultGzipSettings $ RL.logStdout $
  serveWithContext butler underButler (servants cfg pool)

errorFormatters :: ErrorFormatters
errorFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = errorFormatter
  , urlParseErrorFormatter = errorFormatter
  , headerParseErrorFormatter = errorFormatter
  , notFoundErrorFormatter = notFoundFormatter
  }

underButler :: Context '[ErrorFormatters]
underButler = errorFormatters :. EmptyContext

butler :: Proxy API
butler = Proxy

type API = StaticAPI
  :<|> AuthAPI
  :<|> Protect :> CompareAPI

servants :: AppConfig -> RedisPool -> Server API
servants cfg pool = staticServant
  :<|> authServant pool
  :<|> compareServant cfg pool

-- base

type StylesAPI = "styles" :> Raw
type StaticAPI = StylesAPI

staticServant :: Server StaticAPI
staticServant = serveDirectoryWebApp "styles"

-- comparison

data ChooseFormData = ChooseFormData
  { formWinnerId :: Text.Text
  , formLoserId  :: Text.Text
  } deriving (Show)

instance Form.FromForm ChooseFormData where
  fromForm form = ChooseFormData
    <$> Form.parseUnique "winnerId" form
    <*> Form.parseUnique "loserId" form

type CompareTestAPI = "compare" :> "test" :> Get '[JSON] NoContent
type CompareGetAPI = "compare" :> Get '[ServantBlaze.HTML] H.Html
type ComparePostAPI = "compare" :> ReqBody '[FormUrlEncoded] ChooseFormData :> Post '[ServantBlaze.HTML] H.Html

type CompareAPI = EmptyAPI
  :<|> CompareTestAPI
  :<|> CompareGetAPI
  :<|> ComparePostAPI

compareServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server CompareAPI
compareServant cfg pool auth = emptyServer
  :<|> handleTestCompare
  :<|> handleGetCompareData
  :<|> handlePostCompare
  where
    handleTestCompare :: Handler NoContent
    handleTestCompare = do
      MonadIO.liftIO $ putStrLn $ show auth
      -- uid <- initUser pool auth
      -- MonadIO.liftIO $ putStrLn $ "Test endpoint accessed by " ++ show uid
      pure NoContent

    fetchAndBuildSession :: UserId -> App (Either ServerError H.Html)
    fetchAndBuildSession uid = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      mPair <- getNextComparisonPair uid

      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show uid ++ ": " ++ show mPair
      pure $ Right $ mkComparePage uid mPair currentRatings

    handleGetCompareData :: Handler H.Html
    handleGetCompareData = do
      uid <- initUser pool auth "/compare"
      MonadIO.liftIO $ putStrLn $ "Serving /compare for " ++ show uid
      execApp cfg $ fetchAndBuildSession uid

    handlePostCompare :: ChooseFormData -> Handler H.Html
    handlePostCompare submission = do
      uid <- initUser pool auth "/"
      MonadIO.liftIO $ putStrLn $ "Processing POST /compare" ++ show uid ++ " with data: " ++ show submission
      let winnerId = OptionId (TextEnc.encodeUtf8 $ formWinnerId submission)
          loserId  = OptionId (TextEnc.encodeUtf8 $ formLoserId submission)

      execApp cfg $ do
        mWinnerOpt <- getOptionById winnerId
        mLoserOpt  <- getOptionById loserId

        case (mWinnerOpt, mLoserOpt) of
          (Just winnerOpt, Just loserOpt) -> do
            MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show uid ++ ": " ++ show (optionName winnerOpt) ++ " (Win) vs " ++ show (optionName loserOpt)
            updateRatings uid winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show uid
            fetchAndBuildSession uid
          _ -> do
            MonadIO.liftIO $ putStrLn $ "Error: Option ID not found (" ++ show winnerId ++ " or " ++ show loserId ++ ")"
            pure $ Left $ err400
              { errHeaders = [("Content-Type", "text/html; charset=utf-8")]
              , errBody = R.renderHtml $ mkMessageTemplate template
              }
              where
                template = MessageTemplate
                  { messageTitle = "error"
                  , messageHeading = "option id not found"
                  , messageLink = ("/compare", "try again")
                  }

mkComparePage :: UserId -> Maybe (Option, Option) -> [(Option, Double)] -> H.Html
mkComparePage userId mPair rankings =
  pageLayout ("Comparison for " <> TextEnc.decodeUtf8 (unUserId userId)) $ do
    case mPair of
      Just (opt1, opt2) -> mkComparisonSection opt1 opt2
      Nothing -> H.div H.! A.class_ "completion-message" $ "No more pairs to compare based on current strategy."

    H.div H.! A.class_ "results-grid" $ do
      H.div H.! A.class_ "rankings-section" $ do
        H.h2 "Current Rankings"
        mkRankingsTable rankings

mkComparisonSection :: Option -> Option -> H.Html
mkComparisonSection opt1 opt2 = do
  let oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
      oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)
      postUrl = H.textValue $ Text.pack $ "/compare"

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

-- utils

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

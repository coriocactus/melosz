{-# LANGUAGE OverloadedStrings #-}

module Compare where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.ByteString.Char8 as BSC
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
import Auth
import Templates

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

    fetchAndBuildSession :: UserId -> Bool -> App (Either ServerError H.Html)
    fetchAndBuildSession uid isRegistered = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      mPair <- getNextComparisonPair uid

      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show uid ++ ": " ++ show mPair
      pure $ Right $ mkComparePage uid isRegistered mPair currentRatings

    handleGetCompareData :: Handler H.Html
    handleGetCompareData = do
      (uid, isRegistered) <- initUser pool auth "/compare"
      MonadIO.liftIO $ putStrLn $ "Serving /compare for " ++ show uid
      execApp cfg $ fetchAndBuildSession uid isRegistered

    handlePostCompare :: ChooseFormData -> Handler H.Html
    handlePostCompare submission = do
      (uid, isRegistered) <- initUser pool auth "/"
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
            fetchAndBuildSession uid isRegistered
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

mkComparePage :: UserId -> Bool -> Maybe (Option, Option) -> [(Option, Double)] -> H.Html
mkComparePage userId _isRegistered mPair rankings =
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

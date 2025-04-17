{-# LANGUAGE OverloadedStrings #-}

module Compare where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
import Ranking

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

    fetchAndBuildSession :: UserId -> Bool -> Maybe RankMap -> App (Either ServerError H.Html)
    fetchAndBuildSession uid isRegistered maybePrevRankMap = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      mPair <- getNextComparisonPair uid

      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show uid ++ ": " ++ show mPair
      pure $ Right $ mkComparePage uid isRegistered mPair currentRatings maybePrevRankMap

    handleGetCompareData :: Handler H.Html
    handleGetCompareData = do
      (uid, isRegistered) <- initUser pool auth "/compare"
      MonadIO.liftIO $ putStrLn $ "Serving /compare for " ++ show uid
      execApp cfg $ fetchAndBuildSession uid isRegistered Nothing

    handlePostCompare :: ChooseFormData -> Handler H.Html
    handlePostCompare submission = do
      (uid, isRegistered) <- initUser pool auth "/"
      MonadIO.liftIO $ putStrLn $ "Processing POST /compare for " ++ show uid ++ " with data: " ++ show submission
      let winnerId = OptionId (TextEnc.encodeUtf8 $ formWinnerId submission)
          loserId  = OptionId (TextEnc.encodeUtf8 $ formLoserId submission)

      execApp cfg $ do
        optionsList <- Set.toList <$> MonadReader.asks configOptions
        mWinnerOpt <- getOptionById winnerId
        mLoserOpt  <- getOptionById loserId

        case (mWinnerOpt, mLoserOpt) of
          (Just winnerOpt, Just loserOpt) -> do

            prevRatingsList <- getUserRatings uid optionsList
            let prevRankMap = ratingsToRankMap prevRatingsList

            MonadIO.liftIO $ putStrLn $ "Recording comparison for " ++ show uid ++ ": " ++ show (optionName winnerOpt) ++ " (Win) vs " ++ show (optionName loserOpt)
            updateRatings uid winnerOpt loserOpt Win
            MonadIO.liftIO $ putStrLn $ "State updated for " ++ show uid

            fetchAndBuildSession uid isRegistered (Just prevRankMap)
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

mkGuestBanner :: H.Html
mkGuestBanner =
  H.div H.! A.class_ "p-4 mb-6 bg-warning text-warning-content rounded-lg shadow flex items-center justify-between" $ do
    H.span "Guest Access: Rankings will be temporary."
    H.div $ do
      H.a H.! A.href "/login" H.! A.class_ "ds-link ds-link-hover font-semibold mr-4" $ "Login"
      H.a H.! A.href "/register" H.! A.class_ "ds-link ds-link-hover font-semibold" $ "Register"

mkComparePage :: UserId -> Bool -> Maybe (Option, Option) -> [(Option, Double)] -> Maybe RankMap -> H.Html
mkComparePage userId isRegistered mPair currentRatings maybePrevRankMap =
  pageLayout ("Comparison for " <> TextEnc.decodeUtf8 (unUserId userId)) $ do

    Monad.unless isRegistered mkGuestBanner

    case mPair of
      Just (opt1, opt2) -> mkComparisonSection opt1 opt2
      Nothing -> H.div H.! A.class_ "text-center text-lg text-success p-4 bg-success/10 rounded-lg" $ "No more pairs to compare based on current strategy."

    H.div H.! A.class_ "mt-8 p-4 sm:p-6 bg-base-200 rounded-lg shadow" $ do
      H.h2 H.! A.class_ "text-xl font-semibold mb-4" $ "Current Rankings"
      mkRankingsTable currentRatings maybePrevRankMap

mkComparisonSection :: Option -> Option -> H.Html
mkComparisonSection opt1 opt2 = do
  let oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
      oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)
      postUrl = H.textValue $ Text.pack $ "/compare"

  H.div H.! A.class_ "p-4 sm:p-6 bg-base-200 rounded-lg shadow" $ do
    H.div H.! A.class_ "flex flex-col sm:flex-row items-center justify-around w-full gap-4" $ do

      H.form H.! A.class_ "ds-skeleton md:h-96 md:w-96 h-64 w-64" H.! A.method "post" H.! A.action postUrl H.! A.class_ "w-full sm:w-auto" $ do
        H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid1Text)
        H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid2Text)
        H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-primary w-full" $
          H.toHtml (BSC.unpack $ optionName opt1)

      H.span H.! A.class_ "font-bold text-lg my-2 sm:my-0" $ "VS"

      H.form H.! A.class_ "ds-skeleton md:h-96 md:w-96 h-64 w-64" H.! A.method "post" H.! A.action postUrl H.! A.class_ "w-full sm:w-auto" $ do
        H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue oid2Text)
        H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue oid1Text)
        H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-primary w-full" $
          H.toHtml (BSC.unpack $ optionName opt2)

mkRankingsTable :: [(Option, Double)] -> Maybe RankMap -> H.Html
mkRankingsTable currentRatings maybePrevRankMap = H.div H.! A.class_ "overflow-x-auto" $ H.table H.! A.class_ "ds-table w-full" $ do
  H.thead $ H.tr $ do
    H.th H.! A.class_ "text-left p-3" $ "Rank"
    H.th H.! A.class_ "text-left p-3" $ "Option"
    H.th H.! A.class_ "text-left p-3" $ "Rating"
  H.tbody $ do
    let currentWithRanks = ratingsToRankings currentRatings
    mapM_ (mkRankingRow maybePrevRankMap) currentWithRanks
  where
    mkRankingRow :: Maybe RankMap -> (Option, Int) -> H.Html
    mkRankingRow mPrevRankMap (opt, currentRank) = H.tr H.! A.class_ "hover:bg-base-300" $ do

      H.td H.! A.class_ "p-3 font-medium" $ do
         H.toHtml currentRank
         H.toHtml (formatRankChange mPrevRankMap opt currentRank)
      H.td H.! A.class_ "p-3" $ H.toHtml (BSC.unpack $ optionName opt)

      let currentRating = Maybe.fromMaybe 0.0 (lookup opt currentRatings)
      H.td H.! A.class_ "p-3" $ H.toHtml (Printf.printf "%.1f" currentRating :: String)

    formatRankChange :: Maybe RankMap -> Option -> Int -> String
    formatRankChange Nothing _ _ = ""
    formatRankChange (Just prevRankMap) option currentRank =
      let oid = optionId option
          prevRank = Map.findWithDefault currentRank oid prevRankMap
          rankChange = prevRank - currentRank
      in case compare rankChange 0 of
          EQ -> "" :: String
          GT -> Printf.printf " (↑%d)" rankChange
          LT -> Printf.printf " (↓%d)" (abs rankChange)

-- utils

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

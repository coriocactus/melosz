{-# LANGUAGE OverloadedStrings #-}

module Megusta where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Reader as MonadReader
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
import qualified System.Random as Random

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

type MegustaTestAPI = "megusta" :> "test" :> Get '[JSON] NoContent
type MegustaGetAPI = "megusta" :> Get '[ServantBlaze.HTML] H.Html
type MegustaGetAggAPI = "megusta" :> "agg" :> Get '[ServantBlaze.HTML] H.Html
type MegustaPostAPI = "megusta" :> ReqBody '[FormUrlEncoded] ChooseFormData :> Post '[ServantBlaze.HTML] H.Html

type MegustaAPI = EmptyAPI
  :<|> MegustaTestAPI
  :<|> MegustaGetAPI
  :<|> MegustaGetAggAPI
  :<|> MegustaPostAPI

megustaServant :: AppConfig -> RedisPool -> Maybe AuthHeader -> Server MegustaAPI
megustaServant cfg pool auth = emptyServer
  :<|> handleTestMegusta
  :<|> handleGetMegusta
  :<|> handleGetMegustaAgg
  :<|> handlePostMegusta
  where
    handleTestMegusta :: Handler NoContent
    handleTestMegusta = do
      MonadIO.liftIO $ putStrLn $ show auth
      -- uid <- initUser pool auth
      -- MonadIO.liftIO $ putStrLn $ "Test endpoint accessed by " ++ show uid
      pure NoContent

    fetchAndBuildSession :: UserId -> Bool -> App (Either ServerError H.Html)
    fetchAndBuildSession uid isRegistered = do
      MonadIO.liftIO $ putStrLn $ "Fetching data for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      swap <- MonadIO.liftIO Random.randomIO
      mPair <- getNextComparisonPair uid
      currentRatings <- getUserRatings uid optionsList

      MonadIO.liftIO $ putStrLn $ "Next pair for " ++ show uid ++ ": " ++ show mPair
      pure $ Right $ mkMegustaPage uid isRegistered swap mPair currentRatings

    handleGetMegusta :: Handler H.Html
    handleGetMegusta = do
      (uid, isRegistered) <- initUser pool auth "/megusta"
      MonadIO.liftIO $ putStrLn $ "Serving /megusta for " ++ show uid
      execApp cfg $ fetchAndBuildSession uid isRegistered

    fetchAndBuildAggSession :: UserId -> Bool -> App (Either ServerError H.Html)
    fetchAndBuildAggSession uid isRegistered = do
      MonadIO.liftIO $ putStrLn $ "Fetching aggregate rankings for user: " ++ show uid
      optionsList <- Set.toList <$> MonadReader.asks configOptions
      setupUser uid
      currentRatings <- getUserRatings uid optionsList
      pure $ Right $ mkMegustaAggPage uid isRegistered currentRatings

    handleGetMegustaAgg :: Handler H.Html
    handleGetMegustaAgg = do
        (uid, isRegistered) <- initUser pool auth "/megusta/agg"
        MonadIO.liftIO $ putStrLn $ "Serving /megusta/agg for " ++ show uid
        execApp cfg $ fetchAndBuildAggSession uid isRegistered

    handlePostMegusta :: ChooseFormData -> Handler H.Html
    handlePostMegusta submission = do
      (uid, isRegistered) <- initUser pool auth "/megusta"
      MonadIO.liftIO $ putStrLn $ "Processing POST /megusta for " ++ show uid ++ " with data: " ++ show submission
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
                  , messageLink = ("/megusta", "try again")
                  }

mkMegustaPage :: UserId -> Bool -> Bool -> Maybe (Option, Option) -> [(Option, Double)] -> H.Html
mkMegustaPage _userId isRegistered swap mPair currentRatings =
  pageLayout (if isRegistered then User else Guest) "megusta" $ do
    Monad.unless isRegistered $
      H.div H.! A.class_ "text-center ds-alert ds-alert-warning mb-4" $ "guest access: rankings will be temporary"

    H.div H.! A.class_ "flex items-center justify-center" $ do
      case mPair of
        Just (opt1, opt2) -> do
          let (dispOpt1, dispOpt2) = if swap then (opt2, opt1) else (opt1, opt2)
          mkComparisonSection dispOpt1 dispOpt2 currentRatings
        Nothing -> H.div H.! A.class_ "text-center text-lg text-success p-4 bg-success/10 rounded-lg" $ "no more pairs to megusta based on current strategy"

    H.a H.! A.href "/megusta/agg" H.! A.class_ "ds-link ds-link-primary ds-link-hover block text-center mt-4" $ "view rankings"

mkComparisonSection :: Option -> Option -> [(Option, Double)] -> H.Html
mkComparisonSection opt1 opt2 currentRatings = do
  H.div H.! A.class_ "p-4 sm:p-6 bg-base-200 rounded-lg shadow inline-block" $ do
    H.div H.! A.class_ "flex flex-col lg:flex-row items-center justify-around gap-4 sm:gap-8 md:gap-12 lg:gap-16" $ do
      buttonHtml opt1 oid1Text oid2Text
      H.span H.! A.class_ "font-bold text-lg my-4 sm:my-0" $ "VS"
      buttonHtml opt2 oid2Text oid1Text
  where
    oid1Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt1)
    oid2Text = TextEnc.decodeUtf8 (unOptionId $ optionId opt2)
    postUrl = H.textValue $ Text.pack $ "/megusta"
    rankedRatings = ratingsToRankings currentRatings

    findRank :: Option -> String
    findRank opt =
      case lookup opt rankedRatings of
        Just rank -> Printf.printf "#%d" rank
        Nothing   -> ""

    buttonHtml :: Option -> Text.Text -> Text.Text -> H.Html
    buttonHtml opt winnerId loserId =
      H.div H.! A.class_ "ds-indicator" $ do
        H.span H.! A.class_ "ds-indicator-item ds-indicator-top ds-indicator-center ds-badge ds-badge-primary" $ H.toHtml (findRank opt)
        H.form H.! A.class_ "ds-skeleton flex flex-col items-center justify-end p-4 w-[80dvw] lg:w-[40dvw] aspect-square relative transition-none" H.! A.method "post" H.! A.action postUrl $ do
          H.input H.! A.type_ "hidden" H.! A.name "winnerId" H.! A.value (H.textValue winnerId)
          H.input H.! A.type_ "hidden" H.! A.name "loserId" H.! A.value (H.textValue loserId)
          H.button H.! A.type_ "submit" H.! A.class_ "ds-btn ds-btn-primary w-full absolute bottom-0" $ do
            H.toHtml (BSC.unpack $ optionName opt)

mkMegustaAggPage :: UserId -> Bool -> [(Option, Double)] -> H.Html
mkMegustaAggPage _userId isRegistered currentRatings =
  pageLayout (if isRegistered then User else Guest) "megusta rankings" $ do
    Monad.unless isRegistered $
      H.div H.! A.class_ "text-center ds-alert ds-alert-warning mb-2" $ "guest access: rankings will be temporary"
    H.div H.! A.class_ "mt-8 p-4 sm:p-6 bg-base-200 rounded-lg shadow" $ do
      H.h2 H.! A.class_ "text-xl font-semibold mb-4" $ "Current Rankings"
      mkRankingsTableAgg currentRatings
    H.a H.! A.href "/megusta" H.! A.class_ "ds-link ds-link-primary ds-link-hover block text-center mt-4" $ "back"

mkRankingsTableAgg :: [(Option, Double)] -> H.Html
mkRankingsTableAgg currentRatings = H.div H.! A.class_ "overflow-x-auto" $ H.table H.! A.class_ "ds-table w-full" $ do
  H.thead $ H.tr $ do
    H.th H.! A.class_ "text-left p-3" $ "Rank"
    H.th H.! A.class_ "text-left p-3" $ "Option"
    H.th H.! A.class_ "text-left p-3" $ "Rating"
  H.tbody $ do
    let currentWithRanks = ratingsToRankings currentRatings
    mapM_ mkRankingRowAgg currentWithRanks
  where
    mkRankingRowAgg :: (Option, Int) -> H.Html
    mkRankingRowAgg (opt, currentRank) = H.tr H.! A.class_ "hover:bg-base-300" $ do
      H.td H.! A.class_ "p-3 font-medium" $ H.toHtml currentRank
      H.td H.! A.class_ "p-3" $ H.toHtml (BSC.unpack $ optionName opt)
      let currentRating = Maybe.fromMaybe 0.0 (lookup opt currentRatings)
      H.td H.! A.class_ "p-3" $ H.toHtml (Printf.printf "%.1f" currentRating :: String)

-- utils

unUserId :: UserId -> BSC.ByteString
unUserId (UserId bs) = bs

unOptionId :: OptionId -> BSC.ByteString
unOptionId (OptionId bs) = bs

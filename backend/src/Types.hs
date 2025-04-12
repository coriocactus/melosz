-- File: Types.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text.Encoding as TextEnc
import qualified GHC.Generics as Generics
import qualified Web.HttpApiData as HttpApiData

newtype UserId = UserId BS.ByteString
  deriving (Show, Eq, Ord, String.IsString, Generics.Generic)

instance HttpApiData.FromHttpApiData UserId where
  parseUrlPiece text = case HttpApiData.parseUrlPiece text of
    Left err -> Left err
    Right str -> Right (UserId (TextEnc.encodeUtf8 str))

instance Aeson.ToJSON UserId where
  toJSON (UserId bs) = Aeson.String $ TextEnc.decodeUtf8 bs

instance Aeson.FromJSON UserId where
  parseJSON = Aeson.withText "UserId" $ \t -> pure $ UserId (TextEnc.encodeUtf8 t)

newtype OptionId = OptionId BS.ByteString
  deriving (Show, Eq, Ord, String.IsString, Generics.Generic)

instance Aeson.ToJSON OptionId where
  toJSON (OptionId bs) = Aeson.String $ TextEnc.decodeUtf8 bs

instance Aeson.FromJSON OptionId where
  parseJSON = Aeson.withText "OptionId" $ \t -> pure $ OptionId (TextEnc.encodeUtf8 t)

data Option = Option
  { optionId :: OptionId
  , optionName :: BS.ByteString
  , optionSrc :: BS.ByteString
  } deriving (Show, Eq, Ord, Generics.Generic)

instance Aeson.ToJSON Option where
  toJSON opt = Aeson.object
    [ "id"   Aeson..= optionId opt
    , "name" Aeson..= TextEnc.decodeUtf8 (optionName opt)
    , "src" Aeson..= TextEnc.decodeUtf8 (optionSrc opt)
    ]

data Glicko = Glicko
  { glickoRating     :: Double -- mu
  , glickoDeviation  :: Double -- delta
  , glickoVolatility :: Double -- sigma
  } deriving (Show, Eq, Generics.Generic)

instance Aeson.ToJSON Glicko where
  toJSON gp = Aeson.object
    [ "rating"     Aeson..= glickoRating gp
    , "deviation"  Aeson..= glickoDeviation gp
    , "volatility" Aeson..= glickoVolatility gp
    ]

instance Aeson.FromJSON Glicko where
  parseJSON = Aeson.withObject "Glicko" $ \v -> Glicko
    <$> v Aeson..: "rating"
    <*> v Aeson..: "deviation"
    <*> v Aeson..: "volatility"

defaultRating :: Double
defaultRating = 1500.0

defaultDeviation :: Double
defaultDeviation = 350.0

defaultVolatility :: Double
defaultVolatility = 0.06

initialGlicko :: Glicko
initialGlicko = Glicko defaultRating defaultDeviation defaultVolatility

type Relation = Set.Set (Option, Option)

data MatchResult = Win | Loss
  deriving (Show, Eq)

data Comparison = Comparison
  { compUser :: UserId
  , compOption1 :: Option
  , compOption2 :: Option
  , compResult :: MatchResult
  , compTimestamp :: Int
  } deriving (Show, Eq)

createOption :: OptionId -> BS.ByteString -> BS.ByteString -> Option
createOption oid name src = Option oid name src

createUser :: BS.ByteString -> UserId
createUser uid = UserId uid

makeCanonicalPair :: Option -> Option -> (Option, Option)
makeCanonicalPair o1 o2
  | optionId o1 <= optionId o2 = (o1, o2)
  | otherwise                  = (o2, o1)

flipResult :: MatchResult -> MatchResult
flipResult Win  = Loss
flipResult Loss = Win

matchResultToScore :: MatchResult -> Double
matchResultToScore Win = 1.0
matchResultToScore Loss = 0.0

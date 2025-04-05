{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.String as String

newtype UserId = UserId BS.ByteString
  deriving (Show, Eq, Ord, String.IsString)

newtype OptionId = OptionId BS.ByteString
  deriving (Show, Eq, Ord, String.IsString)

data Option = Option
  { optionId :: OptionId
  , optionName :: BS.ByteString
  } deriving (Show, Eq, Ord)

-- Represents win/loss preference: (Winner, Loser)
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

createOption :: OptionId -> BS.ByteString -> Option
createOption oid name = Option oid name

createUser :: BS.ByteString -> UserId
createUser uid = UserId uid

makeCanonicalPair :: Option -> Option -> (Option, Option)
makeCanonicalPair o1 o2
  | optionId o1 <= optionId o2 = (o1, o2)
  | otherwise                  = (o2, o1)

flipResult :: MatchResult -> MatchResult
flipResult Win  = Loss
flipResult Loss = Win

resultToScore :: MatchResult -> Double
resultToScore Win  = 1.0
resultToScore Loss = 0.0

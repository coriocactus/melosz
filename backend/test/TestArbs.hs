module TestArbs where

import qualified Data.ByteString as BS

import Test.QuickCheck (Gen, Arbitrary(..), elements, arbitrary)

import Types

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | ByteString

newtype TestBS = TestBS BS.ByteString
  deriving (Show, Eq)

unTestBS :: TestBS -> BS.ByteString
unTestBS (TestBS bs) = bs

instance Arbitrary TestBS where
  arbitrary = TestBS . BS.pack <$> arbitrary

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Option

newtype TestOption = TestOption Option
  deriving (Show, Eq)

unTestOption :: TestOption -> Option
unTestOption (TestOption opt) = opt

instance Arbitrary TestOption where
  arbitrary = do
    testOid <- arbitrary :: Gen TestOptionId
    testBs <- arbitrary :: Gen TestBS
    return $ TestOption (Option (unTestOptionId testOid) (unTestBS testBs) (unTestBS testBs))

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | OptionId

newtype TestOptionId = TestOptionId OptionId
  deriving (Show, Eq)

unTestOptionId :: TestOptionId -> OptionId
unTestOptionId (TestOptionId oid) = oid

instance Arbitrary TestOptionId where
  arbitrary = do
    testBs <- arbitrary :: Gen TestBS
    return $ TestOptionId (OptionId (unTestBS testBs))

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | MatchResult

newtype TestMatchResult = TestMatchResult MatchResult
  deriving (Show, Eq)

unTestMatchResult :: TestMatchResult -> MatchResult
unTestMatchResult (TestMatchResult mr) = mr

instance Arbitrary TestMatchResult where
  arbitrary = TestMatchResult <$> elements [Win, Loss]

module TestArbs where

import qualified Data.ByteString as BS
import qualified Data.Set as Set

import Test.QuickCheck

import Types

-- | bytestring

newtype TestBS = TestBS BS.ByteString
  deriving (Show, Eq)

unTestBS :: TestBS -> BS.ByteString
unTestBS (TestBS bs) = bs

instance Arbitrary TestBS where
  arbitrary = do
    len <- elements [1..10]
    chars <- vectorOf len (elements ['a'..'z'])
    pure $ TestBS (BS.pack $ map (fromIntegral . fromEnum) chars)

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Option

newtype TestOption = TestOption Option
  deriving (Show, Eq, Ord)

unTestOption :: TestOption -> Option
unTestOption (TestOption opt) = opt

instance Arbitrary TestOption where
  arbitrary = do
    testOid <- arbitrary :: Gen TestOptionId
    testName <- arbitrary :: Gen TestBS
    testSrc <- arbitrary :: Gen TestBS
    return $ TestOption (Option (unTestOptionId testOid) (unTestBS testName) (unTestBS testSrc))

-- | optionid

newtype TestOptionId = TestOptionId OptionId
  deriving (Show, Eq, Ord) -- Added Ord

unTestOptionId :: TestOptionId -> OptionId
unTestOptionId (TestOptionId oid) = oid

instance Arbitrary TestOptionId where
  arbitrary = TestOptionId . OptionId . unTestBS <$> (arbitrary :: Gen TestBS)

-- | matchresult

newtype TestMatchResult = TestMatchResult MatchResult
  deriving (Show, Eq)

unTestMatchResult :: TestMatchResult -> MatchResult
unTestMatchResult (TestMatchResult mr) = mr

instance Arbitrary TestMatchResult where
  arbitrary = TestMatchResult <$> elements [Win, Loss]

-- | userid

newtype TestUserId = TestUserId UserId deriving (Show, Eq, Ord)

unTestUserId :: TestUserId -> UserId
unTestUserId (TestUserId uid) = uid

instance Arbitrary TestUserId where
  arbitrary = TestUserId . UserId . unTestBS <$> (arbitrary :: Gen TestBS)

-- | unique list for options

newtype UniqueOptionList = UniqueOptionList { unUniqueOptionList :: [Option] }
  deriving (Show, Eq)

instance Arbitrary UniqueOptionList where
  arbitrary = do
    opts <- listOf1 (arbitrary :: Gen TestOption) `suchThat` (\l -> length l >= 2)
    let uniqueOpts = fmap unTestOption $ nubOrd $ map TestOption $ map unTestOption opts
    return $ UniqueOptionList uniqueOpts
      where
        nubOrd :: Ord a => [a] -> [a]
        nubOrd = go Set.empty
          where
            go _ [] = []
            go s (x:xs)
              | Set.member x s = go s xs
              | otherwise      = x : go (Set.insert x s) xs

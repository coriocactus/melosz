{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Ranking where
-- reference/citation: https://arxiv.org/abs/1508.06803

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Types

type Rank = Int

type RankMap = Map.Map OptionId Rank

type UserRankMaps = Map.Map UserId RankMap

type SRAResult = [(Int, Double)]

mean :: [Double] -> Double
mean xs
  | null xs = 0.0
  | otherwise = sum xs / fromIntegral (length xs)

variance :: [Double] -> Double
variance xs
  | length xs < 2 = 0.0
  | otherwise = sumOfSquares / fromIntegral (length xs - 1)
  where
    m = mean xs
    sumOfSquares = List.foldl' (\acc x -> acc + (x - m) ** 2) 0.0 xs

ratingsToRankMap :: [(Option, Double)] -> RankMap
ratingsToRankMap sortedRatings =
  Map.fromList $ zipWith (\(opt, _) rank -> (optionId opt, rank)) sortedRatings [1 ..]

-- | Calculate the variance of ranks for a specific item across multiple users
-- | Args: Item's OptionId, Set of UserIds participating, All UserRankMaps
-- | Returns the sample variance, or 0.0 if fewer than 2 users provided a rank
calculateItemRankVariance :: OptionId -> Set.Set UserId -> UserRankMaps -> Double
calculateItemRankVariance itemId userIds userRankMaps = variance ranks
  where
    ranks :: [Double]
    ranks = Maybe.mapMaybe getRankForUser (Set.toList userIds)

    getRankForUser :: UserId -> Maybe Double
    getRankForUser uid =
      fmap fromIntegral (Map.lookup itemId =<< Map.lookup uid userRankMaps)

-- | Calculate the cumulative set S_d of OptionIds present in the top 'd' ranks
calculateCumulativeSetAtDepth :: Int -> UserRankMaps -> Set.Set OptionId
calculateCumulativeSetAtDepth d userRankMaps =
  Map.foldl' mergeTopItems Set.empty userRankMaps
  where
    mergeTopItems :: Set.Set OptionId -> RankMap -> Set.Set OptionId
    mergeTopItems currentSet rankMap = Set.union currentSet (topItems rankMap)

    topItems :: RankMap -> Set.Set OptionId
    topItems rankMap = Map.keysSet $ Map.filter (<= d) rankMap

-- | Calculate the Sequential Rank Agreement curve
-- |
-- | Args:
-- | - allOptions: A set of all possible options being ranked
-- | - userRankMaps: A map from UserId to their RankMap. Assumes fully ranked
-- |
-- | Returns:
-- | - SRAResult: A list of (depth, sra_value) pairs, from depth 1 to n_options
-- | - Empty list if there are fewer than 2 users or no options
calculateSRA :: Set.Set Option -> UserRankMaps -> SRAResult
calculateSRA allOptions userRankMaps =
  let userIds = Map.keysSet userRankMaps
      numUsers = Set.size userIds
      numOptions = Set.size allOptions
      maxDepth = numOptions

      isValid = numUsers >= 2 && numOptions > 0
   in if not isValid
        then []
        else map calculateSraAtDepth [1 .. maxDepth]
  where
    calculateSraAtDepth :: Int -> (Int, Double)
    calculateSraAtDepth d =
      let cumulativeSetIds = calculateCumulativeSetAtDepth d userRankMaps
          setSize = Set.size cumulativeSetIds
       in if setSize == 0
            then (d, 0.0) -- or potentially nan, but 0 seems reasonable for no items
            else
              let sumOfVariances = sum $ map getItemVariance (Set.toList cumulativeSetIds)
                  avgVariance = sumOfVariances / fromIntegral setSize
               in (d, sqrt avgVariance)

    getItemVariance :: OptionId -> Double
    getItemVariance itemId =
      calculateItemRankVariance itemId (Map.keysSet userRankMaps) userRankMaps

goldenMain :: IO ()
goldenMain = do
  putStrLn "--- SRA Example based on Paper Table 1 ---"

  let optA = createOption "A" "A" ""
      optB = createOption "B" "B" ""
      optC = createOption "C" "C" ""
      optD = createOption "D" "D" ""
      optE = createOption "E" "E" ""

  let allOptionsSet = Set.fromList [optA, optB, optC, optD, optE]

  let user1 = "User1" :: UserId
      user2 = "User2" :: UserId
      user3 = "User3" :: UserId

  -- Ranks from Table 1, panel (b)
  let ranks1 = Map.fromList [(optionId optA, 1), (optionId optB, 2), (optionId optC, 3), (optionId optD, 4), (optionId optE, 5)]
      ranks2 = Map.fromList [(optionId optA, 1), (optionId optC, 2), (optionId optD, 3), (optionId optB, 4), (optionId optE, 5)]
      ranks3 = Map.fromList [(optionId optB, 1), (optionId optA, 2), (optionId optE, 3), (optionId optC, 4), (optionId optD, 5)]

  let userRankMapsExample = Map.fromList [(user1, ranks1), (user2, ranks2), (user3, ranks3)]

  let sraResult = calculateSRA allOptionsSet userRankMapsExample

  putStrLn "Calculated SRA curve:"
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult

  putStrLn "\nExpected values (approx):"
  putStrLn "Depth 1: SRA = 1.155"
  putStrLn "Depth 2: SRA = 1.106"
  putStrLn "Depth 3: SRA = 1.095"
  putStrLn "Depth 4: SRA = 1.095"
  putStrLn "Depth 5: SRA = 1.095"

testMain :: IO ()
testMain = do
  putStrLn "--- SRA Example (Bilateral) ---"

  let optA = createOption "A" "A" ""
      optB = createOption "B" "B" ""
      optC = createOption "C" "C" ""
      optD = createOption "D" "D" ""
      optE = createOption "E" "E" ""

  let allOptionsSet = Set.fromList [optA, optB, optC, optD, optE]

  let user1 = "User1" :: UserId
      user2 = "User2" :: UserId
      user3 = "User3" :: UserId

  let ranks1 = Map.fromList [(optionId optA, 1), (optionId optB, 2), (optionId optC, 3), (optionId optD, 4), (optionId optE, 5)]
      ranks2 = Map.fromList [(optionId optA, 1), (optionId optC, 2), (optionId optD, 3), (optionId optB, 4), (optionId optE, 5)]
      ranks3 = Map.fromList [(optionId optB, 1), (optionId optA, 2), (optionId optE, 3), (optionId optC, 4), (optionId optD, 5)]

  let userRankMapsExample12 = Map.fromList [(user1, ranks1), (user2, ranks2)]
  let userRankMapsExample13 = Map.fromList [(user1, ranks1), (user3, ranks3)]
  let userRankMapsExample32 = Map.fromList [(user3, ranks3), (user2, ranks2)]

  let sraResult12 = calculateSRA allOptionsSet userRankMapsExample12
  putStrLn "Calculated SRA curve:"
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult12

  let sraResult13 = calculateSRA allOptionsSet userRankMapsExample13
  putStrLn "Calculated SRA curve:"
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult13

  let sraResult32 = calculateSRA allOptionsSet userRankMapsExample32
  putStrLn "Calculated SRA curve:"
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult32

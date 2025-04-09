{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Ranking where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Ord as Ord

import Types

type Rank = Int

type RankMap = Map.Map OptionId Rank

type UserRankMaps = Map.Map UserId RankMap

type AggregatedScores = Map.Map OptionId Double

type ConsensusRanking = [(OptionId, Double)]

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Sequential Rank Agreement (SRA)
-- | Reference: https://arxiv.org/abs/1508.06803

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
  in if not isValid then [] else map calculateSraAtDepth [1 .. maxDepth]
  where
    calculateSraAtDepth :: Int -> (Int, Double)
    calculateSraAtDepth d =
      let cumulativeSetIds = calculateCumulativeSetAtDepth d userRankMaps
          setSize = Set.size cumulativeSetIds
      in if setSize == 0 then (d, 0.0) else
            let sumOfVariances = sum $ map getItemVariance (Set.toList cumulativeSetIds)
                avgVariance = sumOfVariances / fromIntegral setSize
            in (d, sqrt avgVariance)

    getItemVariance :: OptionId -> Double
    getItemVariance itemId =
      calculateItemRankVariance itemId (Map.keysSet userRankMaps) userRankMaps

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Rank Aggregation Methods

-- | Helper to convert aggregated scores to a sorted consensus ranking.
scoresToConsensus :: AggregatedScores -> ConsensusRanking
scoresToConsensus scores = List.sortBy (Ord.comparing (Ord.Down . snd)) (Map.toList scores)

-- | Helper to get all unique OptionIds from UserRankMaps.
getAllOptionIds :: UserRankMaps -> Set.Set OptionId
getAllOptionIds userRankMaps = Map.foldl' (\acc rankMap -> Set.union acc (Map.keysSet rankMap)) Set.empty userRankMaps

-- | Helper to initialize scores for all options to zero.
initializeScores :: UserRankMaps -> AggregatedScores
initializeScores userRankMaps = Map.fromSet (const 0.0) (getAllOptionIds userRankMaps)

-- | Aggregate rankings based on a positional scoring rule (weights assigned by rank).
-- | Args:
-- | - userRankMaps: Map of UserId to their RankMap.
-- | - weights: A list of scores, where the element at index `r-1` is the score for rank `r`.
-- |            Length must be >= number of options.
-- | Returns: ConsensusRanking sorted by score descending.
aggregateByScoringRule :: UserRankMaps -> [Double] -> ConsensusRanking
aggregateByScoringRule userRankMaps weights
  | Map.null userRankMaps = []
  | otherwise = scoresToConsensus aggregated
  where
    initialScoresMap = initializeScores userRankMaps
    aggregated = Map.foldl' accumulateUserScores initialScoresMap userRankMaps

    accumulateUserScores :: AggregatedScores -> RankMap -> AggregatedScores
    accumulateUserScores currentScores rankMap =
      Map.foldlWithKey' (accumulateOptionScore rankMap) currentScores currentScores

    accumulateOptionScore :: RankMap -> AggregatedScores -> OptionId -> Double -> AggregatedScores
    accumulateOptionScore userRankMap currentScores optId _ =
      case Map.lookup optId userRankMap of
        Nothing -> currentScores -- Option not ranked by this user
        Just rank -> Map.adjust (+ scoreToAdd) optId currentScores
          where scoreToAdd = if rank > 0 && rank <= length weights then weights !! (rank - 1) else 0.0 -- Assign 0 if rank is out of bounds or invalid

-- | Plurality Voting: Only the first choice gets a point.
-- | Args:
-- | - numOptions: Total number of options being ranked.
-- | - userRankMaps: Map of UserId to their RankMap.
-- | Returns: ConsensusRanking sorted by score descending.
aggregatePlurality :: Int -> UserRankMaps -> ConsensusRanking
aggregatePlurality numOptions userRankMaps =
  let weights = 1.0 : replicate (numOptions - 1) 0.0
  in aggregateByScoringRule userRankMaps weights

-- | Veto Voting: Every option gets a point unless it's ranked last.
-- | Args:
-- | - numOptions: Total number of options being ranked.
-- | - userRankMaps: Map of UserId to their RankMap.
-- | Returns: ConsensusRanking sorted by score descending.
aggregateVeto :: Int -> UserRankMaps -> ConsensusRanking
aggregateVeto numOptions userRankMaps =
  let weights = replicate (numOptions - 1) 1.0 ++ [0.0]
  in aggregateByScoringRule userRankMaps weights

-- | Borda Count: Rank `r` gets `n - r` points (where `n` is numOptions).
-- | Rank 1 gets n-1, Rank 2 gets n-2, ..., Rank n gets 0.
-- | Args:
-- | - numOptions: Total number of options being ranked.
-- | - userRankMaps: Map of UserId to their RankMap.
-- | Returns: ConsensusRanking sorted by score descending.
aggregateBorda :: Int -> UserRankMaps -> ConsensusRanking
aggregateBorda numOptions userRankMaps =
  let weights = map fromIntegral $ reverse [0 .. numOptions - 1]
  in aggregateByScoringRule userRankMaps weights


-- | Kemeny-Young Method: Finds the consensus ranking that minimizes the sum of
-- | Kendall tau distances to the individual user rankings.
-- | WARNING: Computationally expensive O(m * k! * k^2), where m=users, k=options. Use with caution.
-- | Args:
-- | - allOptions: Set of all OptionIds being ranked.
-- | - userRankMaps: Map of UserId to their RankMap.
-- | Returns: The Kemeny-Young consensus ranking (best score is lowest distance), or empty if no options/users.
aggregateKemenyYoung :: Set.Set OptionId -> UserRankMaps -> ConsensusRanking
aggregateKemenyYoung allOptionIds userRankMaps
  | Set.null allOptionIds || Map.null userRankMaps = []
  | otherwise = map (\(oid, rank) -> (oid, fromIntegral rank)) bestRankingList
  where
    optionList = Set.toList allOptionIds
    allPermutations = List.permutations optionList
    numOptions = length optionList

    -- Convert each permutation into a RankMap for comparison
    permutationToRankMap :: [OptionId] -> RankMap
    permutationToRankMap p = Map.fromList $ zip p [1..numOptions]

    -- Calculate Kendall tau distance between two rankings
    kendallTauDistance :: RankMap -> RankMap -> Int
    kendallTauDistance r1 r2 =
      let pairs = [(o1, o2) | o1 <- optionList, o2 <- optionList, o1 < o2]
          discordantCount = foldl (countDiscordant r1 r2) 0 pairs
      in discordantCount

    countDiscordant :: RankMap -> RankMap -> Int -> (OptionId, OptionId) -> Int
    countDiscordant r1 r2 currentCount (o1, o2) =
      case (Map.lookup o1 r1, Map.lookup o2 r1, Map.lookup o1 r2, Map.lookup o2 r2) of
        (Just rank1_o1, Just rank1_o2, Just rank2_o1, Just rank2_o2) ->
            let concordant = (rank1_o1 < rank1_o2 && rank2_o1 < rank2_o2) || (rank1_o1 > rank1_o2 && rank2_o1 > rank2_o2)
            in if concordant then currentCount else currentCount + 1
        _ -> currentCount -- Pair involves unranked item, skip

    totalDistance :: RankMap -> Int
    totalDistance candidateRankMap =
      sum $ map (kendallTauDistance candidateRankMap) (Map.elems userRankMaps)

    evaluatedPermutations :: [(RankMap, Int)]
    evaluatedPermutations = map (\p -> let rm = permutationToRankMap p in (rm, totalDistance rm)) allPermutations

    minDistance = minimum $ map snd evaluatedPermutations
    bestRankMaps = map fst $ filter ((== minDistance) . snd) evaluatedPermutations

    -- Convert the best RankMap back to a sorted list format [(OptionId, Rank)]
    -- We use the *negative* distance as the score, so higher is better (less distance).
    bestRankingList = case bestRankMaps of
      [] -> [] -- Should not happen if evaluatedPermutations is not empty
      (bestMap:_) -> List.sortBy (Ord.comparing snd) $ Map.toList bestMap

-- ===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|===|
-- | Test Main

goldenMain :: IO ()
goldenMain = do
  putStrLn "--- SRA Example based on Paper Table 1 ---"

  let optA = createOption "A" "A" ""
      optB = createOption "B" "B" ""
      optC = createOption "C" "C" ""
      optD = createOption "D" "D" ""
      optE = createOption "E" "E" ""

  let allOptionsSet = Set.fromList [optA, optB, optC, optD, optE]
  let allOptionIdsSet = Set.map optionId allOptionsSet

  let user1 = "User1" :: UserId
      user2 = "User2" :: UserId
      user3 = "User3" :: UserId

  let ranks1 = Map.fromList [(optionId optA, 1), (optionId optB, 2), (optionId optC, 3), (optionId optD, 4), (optionId optE, 5)]
      ranks2 = Map.fromList [(optionId optA, 1), (optionId optC, 2), (optionId optD, 3), (optionId optB, 4), (optionId optE, 5)]
      ranks3 = Map.fromList [(optionId optB, 1), (optionId optA, 2), (optionId optE, 3), (optionId optC, 4), (optionId optD, 5)]

  let userRankMapsExample = Map.fromList [(user1, ranks1), (user2, ranks2), (user3, ranks3)]
  let numOpts = Set.size allOptionIdsSet

  putStrLn "\n--- Rank Aggregation Examples ---"

  putStrLn "\nPlurality:"
  mapM_ print (aggregatePlurality numOpts userRankMapsExample)

  putStrLn "\nVeto:"
  mapM_ print (aggregateVeto numOpts userRankMapsExample)

  putStrLn "\nBorda Count:"
  mapM_ print (aggregateBorda numOpts userRankMapsExample)

  putStrLn "\nKemeny-Young (Ranking):"
  mapM_ print (aggregateKemenyYoung allOptionIdsSet userRankMapsExample)

  putStrLn "\n--- SRA ---"
  let sraResult = calculateSRA allOptionsSet userRankMapsExample

  putStrLn "Calculated SRA curve:"
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult

  putStrLn "\nExpected SRA values (approx):"
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

  putStrLn "\nSRA Curve (User 1 vs User 2):"
  let sraResult12 = calculateSRA allOptionsSet userRankMapsExample12
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult12

  putStrLn "\nSRA Curve (User 1 vs User 3):"
  let sraResult13 = calculateSRA allOptionsSet userRankMapsExample13
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult13

  putStrLn "\nSRA Curve (User 3 vs User 2):"
  let sraResult32 = calculateSRA allOptionsSet userRankMapsExample32
  mapM_ (\(depth, sra) -> putStrLn $ "Depth " ++ show depth ++ ": SRA = " ++ show sra) sraResult32

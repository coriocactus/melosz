module Glicko2 where

import Types

-- Glicko-2 Constants
_q :: Double
_q = log 10 / 400 -- 0.00575646273

-- Helper function g(RD)
g :: Double -> Double
g rd = 1 / sqrt (1 + 3 * _q * _q * rd * rd / (pi * pi))

-- Expected outcome E(rating1, rating2, RD2)
expectedOutcome :: Double -> Double -> Double -> Double
expectedOutcome r1 r2 rd2 = 1 / (1 + 10 ** (g rd2 * (r1 - r2) / (-400)))

-- Estimated variance v(rating1, rating2, RD2), note: this 'v' is different from volatility update 'v'
estimatedVariance :: Double -> Double -> Double -> Double
estimatedVariance r1 r2 rd2 =
  let e = expectedOutcome r1 r2 rd2
      g_rd2 = g rd2
  in 1 / (_q * _q * g_rd2 * g_rd2 * e * (1 - e))

-- Estimated improvement delta - Step 5 intermediate calculation component
-- delta = v * Sum[g(RDj)*(sj-Ej)], where v = 1 / Sum[g(RDj)^2*Ej*(1-Ej)] = 1 / d_squared_inv
estimatedImprovement :: Double -> Double -> Double -> Double -> Double
estimatedImprovement d_squared_inv opponent_g score expected =
  (1.0 / d_squared_inv) * opponent_g * (score - expected)

-- New volatility calculation function
updateVolatility :: Double -> Double -> Double -> Double -> Double -> Double
updateVolatility delta rd v sigma tau =
  -- delta here refers to the overall estimated improvement = v * Sum[g(RDj)*(sj-Ej)]
  -- v here refers to the estimated variance of the rating = 1 / Sum[g(RDj)^2*Ej*(1-Ej)]
  let a = log (sigma ** 2)
      epsilon = 0.000001 -- Convergence tolerance

      f x = (exp x * (delta ** 2 - rd ** 2 - v - exp x)) / (2 * (rd ** 2 + v + exp x) ** 2) - (x - a) / (tau ** 2)

      iterateSigma' :: Double -> Double -> Double -> Int -> Double
      iterateSigma' current_a lower_bound upper_bound iter
        | iter <= 0 = exp (current_a / 2)
        | otherwise =
            let next_sigma' = exp (current_a / 2)
                f_a = f current_a
            in if abs f_a < epsilon then next_sigma'
               else let next_a = if f_a > 0 then (current_a + lower_bound) / 2 else (current_a + upper_bound) / 2
                        (new_lower, new_upper) = if f_a > 0 then (lower_bound, current_a) else (current_a, upper_bound)
                    in iterateSigma' next_a new_lower new_upper (iter - 1)

      initial_k = 1.0
      -- Note: Glickman's paper uses 'Delta' for the estimated improvement term in the f(x) function.
      -- Our 'delta' parameter corresponds to this 'Delta'.
      b | delta**2 > rd**2 + v = log(delta**2 - rd**2 - v)
        | otherwise            = findInitialB a initial_k

      findInitialB :: Double -> Double -> Double
      findInitialB current_a k
        | f (current_a - k * tau) < 0 = findInitialB current_a (k + 1.0)
        | otherwise                   = current_a - k * tau

  in iterateSigma' a b a 100

-- Pre-rating period RD calculation
preRatingRD :: Double -> Double -> Double
preRatingRD rd sigma_prime = sqrt (rd * rd + sigma_prime * sigma_prime)

-- Update Rating (Glickman Step 6 & 7 combined)
updateRating :: Double -> Double -> Double -> Double -> Double -> Double -> Double
updateRating r rd_star opponent_r opponent_rd score d_squared_inv =
  -- For single match: Sum[g(RDj)*(sj-Ej)] just becomes g(RD_opp)*(s - E)
  let g_rd_opp = g opponent_rd
      e = expectedOutcome r opponent_r opponent_rd
      improvement_sum = g_rd_opp * (score - e)
  in r + _q / (1 / (rd_star * rd_star) + d_squared_inv) * improvement_sum

-- Update RD (Glickman Step 6 & 8 combined)
-- Accepts rd_star (pre-rating period RD) and d_squared_inv (calculated from opponents)
updateRD :: Double -> Double -> Double
updateRD rd_star d_squared_inv =
  sqrt $ 1 / (1 / (rd_star * rd_star) + d_squared_inv)

-- Calculate new Glicko-2 parameters after one match (simplified per-match update)
-- Takes the two glickos, the outcome for glicko 1, and system tau
-- Returns the updated (glicko1, glicko2)
calculateNewRatings :: Glicko -> Glicko -> MatchResult -> Double -> (Glicko, Glicko)
calculateNewRatings p1 p2 result1 tau =
  let r1 = glickoRating p1
      rd1 = glickoDeviation p1
      sigma1 = glickoVolatility p1

      r2 = glickoRating p2
      rd2 = glickoDeviation p2
      sigma2 = glickoVolatility p2

      score1 = matchResultToScore result1
      score2 = 1.0 - score1 -- Score for glicko 2

      -- Calculations FOR Glicko 1 (opponent is Glicko 2)
      g_rd2 = g rd2
      e1 = expectedOutcome r1 r2 rd2
      -- d^2_inv for glicko 1 = q^2 * g(RD2)^2 * E1 * (1 - E1)
      d_squared_inv1 = _q * _q * g_rd2 * g_rd2 * e1 * (1 - e1)
      -- Estimated improvement delta1 = (1/d^2_inv1) * g(RD2)*(s1 - E1)
      delta1 = (1.0 / d_squared_inv1) * g_rd2 * (score1 - e1)
      -- Estimated variance v1 = 1/d^2_inv1
      v1 = 1.0 / d_squared_inv1

      -- Calculations FOR Glicko 2 (opponent is Glicko 1)
      g_rd1 = g rd1
      e2 = expectedOutcome r2 r1 rd1
      -- d^2_inv for glicko 2 = q^2 * g(RD1)^2 * E2 * (1 - E2)
      d_squared_inv2 = _q * _q * g_rd1 * g_rd1 * e2 * (1 - e2)
      -- Estimated improvement delta2 = (1/d^2_inv2) * g(RD1)*(s2 - E2)
      delta2 = (1.0 / d_squared_inv2) * g_rd1 * (score2 - e2)
       -- Estimated variance v2 = 1/d^2_inv2
      v2 = 1.0 / d_squared_inv2

      -- Update volatilities (Step 5) - Pass delta and v
      sigma1_prime = updateVolatility delta1 rd1 v1 sigma1 tau
      sigma2_prime = updateVolatility delta2 rd2 v2 sigma2 tau

      -- Pre-rating period RDs (Step 6)
      rd1_star = preRatingRD rd1 sigma1_prime
      rd2_star = preRatingRD rd2 sigma2_prime

      -- New ratings (Step 7) - Pass necessary components
      r1_prime = updateRating r1 rd1_star r2 rd2 score1 d_squared_inv1
      r2_prime = updateRating r2 rd2_star r1 rd1 score2 d_squared_inv2

      -- New RDs (Step 8) - Calls now match the corrected updateRD signature
      rd1_prime = updateRD rd1_star d_squared_inv1
      rd2_prime = updateRD rd2_star d_squared_inv2


      updated_p1 = Glicko (clampRating r1_prime) (clampRD rd1_prime) sigma1_prime
      updated_p2 = Glicko (clampRating r2_prime) (clampRD rd2_prime) sigma2_prime

  in (updated_p1, updated_p2)

clampRating :: Double -> Double
clampRating r = max 100 r

clampRD :: Double -> Double
clampRD rd = max 30 (min 350 rd)

-- normalization
glickoToDisplay :: Glicko -> Glicko
glickoToDisplay glicko = Glicko
  (173.7178 * (glickoRating glicko - 1500) / 400 + 1500)
  (173.7178 * glickoDeviation glicko / 400)
  (glickoVolatility glicko)

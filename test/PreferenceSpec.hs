{-# LANGUAGE OverloadedStrings #-}

module PreferenceSpec where

import qualified Control.Monad.State as MonadState
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Test.Hspec

import Types
import AppState
import Preference

import TestUtils

spec :: Spec
spec = describe "Preference" $ do

  let prefAB = Set.singleton (optA, optB) :: Relation
      prefsABCycle = Set.fromList [(optA, optB), (optB, optC), (optC, optA)] :: Relation
      prefsAB_BC = Set.fromList [(optA, optB), (optB, optC)] :: Relation

  describe "isPreferred" $ do
    it "returns True if preference exists" $
      isPreferred optA optB prefAB `shouldBe` True
    it "returns False if preference does not exist" $
      isPreferred optB optA prefAB `shouldBe` False
    it "returns False for unrelated options" $
      isPreferred optA optC prefAB `shouldBe` False

  describe "hasBeenCompared" $ do
    it "returns True if A > B exists" $
      hasBeenCompared optA optB prefAB `shouldBe` True
    it "returns True if B > A exists" $
      hasBeenCompared optB optA prefAB `shouldBe` True
    it "returns False if neither preference exists" $
      hasBeenCompared optA optC prefAB `shouldBe` False

  describe "canonicalizeViolation" $ do
    it "correctly canonicalizes A > B, B > C, C > A" $
      canonicalizeViolation (optA, optC, optB) `shouldBe` (optA, optC, optB)
    it "correctly canonicalizes B > C, C > A, A > B" $
      canonicalizeViolation (optB, optA, optC) `shouldBe` (optA, optC, optB)
    it "correctly canonicalizes C > A, A > B, B > C" $
      canonicalizeViolation (optC, optB, optA) `shouldBe` (optA, optC, optB)

  describe "referencesEdge" $ do
    let cycleABC = (optA, optC, optB)
    it "returns True if edge (A,B) is referenced (as A > B)" $
      referencesEdge optA optB cycleABC `shouldBe` True
    it "returns True if edge (B,C) is referenced (as B > C)" $
      referencesEdge optB optC cycleABC `shouldBe` True
    it "returns True if edge (C,A) is referenced (as C > A)" $
      referencesEdge optC optA cycleABC `shouldBe` True
    it "returns False if edge (B,A) is referenced" $
      referencesEdge optB optA cycleABC `shouldBe` False
    it "returns False if edge (D,A) is referenced" $
      referencesEdge optD optA cycleABC `shouldBe` False

  describe "findPairsInViolations" $ do
    it "finds all pairs involved in a single violation" $ do
      let violation = canonicalizeViolation (optA, optC, optB)
          violationsSet = Set.singleton violation
          expectedPairs = Set.fromList
            [ makeCanonicalPair optA optB
            , makeCanonicalPair optB optC
            , makeCanonicalPair optC optA
            ]
      findPairsInViolations violationsSet `shouldBe` expectedPairs
    it "finds unique pairs from multiple violations" $ do
      let violation1 = canonicalizeViolation (optA, optC, optB)
          violation2 = canonicalizeViolation (optA, optD, optB)
          violationsSet = Set.fromList [violation1, violation2]
          expectedPairs = Set.fromList
            [ makeCanonicalPair optA optB
            , makeCanonicalPair optB optC
            , makeCanonicalPair optC optA
            , makeCanonicalPair optB optD
            , makeCanonicalPair optD optA
            ]
      findPairsInViolations violationsSet `shouldBe` expectedPairs

  let setupState user prefs violations uncompared options = initialState
          { stateUsers = Set.singleton user
          , stateOptions = options
          , statePreferences = Map.singleton user prefs
          , stateViolations = Map.singleton user violations
          , stateUncomparedPairs = Map.singleton user uncompared
          }
      allPairs = getAllOptionPairsSet allTestOptionsSet
      cycleViolationABC = canonicalizeViolation (optA, optC, optB)

  describe "recordComparison" $ do
    it "adds a new preference (Win)" $ do
      let startState = setupState testUser1 Set.empty Set.empty allPairs (Set.fromList [optA, optB, optC])
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      let prefs = statePreferences finalState Map.! testUser1
      Set.member (optA, optB) prefs `shouldBe` True

    it "removes the canonical pair from uncompared pairs" $ do
      let startState = setupState testUser1 Set.empty Set.empty allPairs (Set.fromList [optA, optB, optC])
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      let uncompared = stateUncomparedPairs finalState Map.! testUser1
      Set.member (makeCanonicalPair optA optB) uncompared `shouldBe` False
      Set.size uncompared `shouldBe` (Set.size allPairs - 1)

    it "replaces an existing reverse preference" $ do
      let startPrefs = Set.singleton (optB, optA)
          startState = setupState testUser1 startPrefs Set.empty (Set.delete (makeCanonicalPair optA optB) allPairs) (Set.fromList [optA, optB, optC])
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      let prefs = statePreferences finalState Map.! testUser1
      Set.member (optA, optB) prefs `shouldBe` True
      Set.member (optB, optA) prefs `shouldBe` False
      Set.size prefs `shouldBe` 1

    it "increments the timestamp" $ do
      let startState = initialState { stateNextTimestamp = 5 }
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      stateNextTimestamp finalState `shouldBe` 6

  describe "updateViolationsOnNewPreference" $ do
    it "detects a new violation (A > B added to B > C, C > A)" $ do
      let startPrefs = Set.fromList [(optB, optC), (optC, optA)]
          startState = setupState testUser1 startPrefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
      let newPref = (optA, optB)
          finalPrefs = Set.insert newPref startPrefs
          action = MonadState.put (startState { statePreferences = Map.singleton testUser1 finalPrefs }) >> updateViolationsOnNewPreference testUser1 newPref Nothing
      finalState <- execAppTest action Nothing (Just startState)
      let violations = stateViolations finalState Map.! testUser1
      Set.null violations `shouldBe` False
      Set.size violations `shouldBe` 1
      Set.findMin violations `shouldBe` canonicalizeViolation (optA, optC, optB)

    it "removes violations when a preference is reversed" $ do
      let cycleViolation = canonicalizeViolation (optA, optC, optB)
          startPrefs = Set.fromList [(optA, optB), (optB, optC), (optC, optA)]
          startViolations = Set.singleton cycleViolation
          startState = setupState testUser1 startPrefs startViolations Set.empty (Set.fromList [optA, optB, optC])

      let removedPref = (optC, optA)
          newPref = (optA, optC)
          finalPrefs = Set.insert newPref (Set.delete removedPref startPrefs)
          action = MonadState.put (startState { statePreferences = Map.singleton testUser1 finalPrefs }) >> updateViolationsOnNewPreference testUser1 newPref (Just removedPref)

      finalState <- execAppTest action Nothing (Just startState)
      let violations = stateViolations finalState Map.! testUser1
      Set.null violations `shouldBe` True

  describe "checkIfComplete" $ do
    it "returns True when uncompared and violations are empty" $ do
      let state = setupState testUser1 prefsAB_BC Set.empty Set.empty (Set.fromList [optA, optB, optC])
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` True
    it "returns False when uncompared is not empty" $ do
      let state = setupState testUser1 prefsAB_BC Set.empty (Set.singleton (makeCanonicalPair optA optC)) (Set.fromList [optA, optB, optC])
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` False
    it "returns False when violations is not empty" $ do
      let violation = canonicalizeViolation (optA, optC, optB)
          state = setupState testUser1 prefsABCycle (Set.singleton violation) Set.empty (Set.fromList [optA, optB, optC])
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` False

  describe "calculateTransitivityScore" $ do
    it "returns 1.0 for fewer than 3 options" $ do
      let state2opts = setupState testUser1 Set.empty Set.empty Set.empty (Set.fromList [optA, optB])
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state2opts)
      score `shouldBeApprox` 1.0

    it "returns 1.0 for 3 options with no violations" $ do
      let state3opts = setupState testUser1 prefsAB_BC Set.empty Set.empty (Set.fromList [optA, optB, optC])
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state3opts)
      score `shouldBeApprox` 1.0

    it "returns 0.0 for 3 options with 1 violation (the only possible triple)" $ do
      let state3optsViol = setupState testUser1 prefsABCycle (Set.singleton cycleViolationABC) Set.empty (Set.fromList [optA, optB, optC])
          -- n=3, total triples = 3*2*1 / 6 = 1. Score = 1 - 1/1 = 0
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state3optsViol)
      score `shouldBeApprox` 0.0

    it "calculates correctly for 4 options with 1 violation" $ do
      let state4optsViol = setupState testUser1 prefsABCycle (Set.singleton cycleViolationABC) Set.empty allTestOptionsSet
          -- n=4, total triples = 4*3*2 / 6 = 4. Score = 1 - 1/4 = 0.75
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state4optsViol)
      score `shouldBeApprox` 0.75

    it "calculates correctly for 4 options with 2 violations" $ do
      let cycleViolationABD = canonicalizeViolation (optA, optD, optB) -- A>B, B>D, D>A (assuming these prefs exist)
          violations = Set.fromList [cycleViolationABC, cycleViolationABD]
          -- Dummy prefs, actual prefs don't influence score directly, only violations count
          state4opts2Viol = setupState testUser1 prefsABCycle violations Set.empty allTestOptionsSet
          -- n=4, total triples = 4. Score = 1 - 2/4 = 0.5
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state4opts2Viol)
      score `shouldBeApprox` 0.5

  describe "calculateAgreementScore" $ do
    let eloRankPerfect = [(optA, 1700.0), (optB, 1600.0), (optC, 1500.0)]
        eloRankTies = [(optA, 1600.0), (optB, 1600.0), (optC, 1500.0)]

    it "returns 1.0 (max agreement) when no preferences exist" $ do
      let state = setupState testUser1 Set.empty Set.empty Set.empty allTestOptionsSet
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

    it "returns 1.0 (max agreement) when preferences perfectly match Elo order" $ do
      -- Prefs: A>B, A>C, B>C (total 3)
      -- Elo: A>B, A>C, B>C (3 concordant, 0 discordant)
      -- Tau = (3 - 0) / 3 = 1. Score = (1 + 1) / 2 = 1.0
      let prefs = Set.fromList [(optA, optB), (optA, optC), (optB, optC)]
          state = setupState testUser1 prefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

    it "returns 0.0 (max disagreement) when preferences perfectly oppose Elo order" $ do
      -- Prefs: C>B, C>A, B>A (total 3)
      -- Elo: A>B, A>C, B>C (0 concordant, 3 discordant)
      -- Tau = (0 - 3) / 3 = -1. Score = (-1 + 1) / 2 = 0.0
      let prefs = Set.fromList [(optC, optB), (optC, optA), (optB, optA)]
          state = setupState testUser1 prefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 0.0

    it "returns 0.5 (no correlation) for mixed preferences" $ do
      -- Prefs: A>B, C>A (total 2)
      -- Elo (perfect): A>B (conc), A>C (disc), B>C (-)
      -- Check A>B vs Elo A>B: Concordant (1)
      -- Check C>A vs Elo A>C: Discordant (1)
      -- Tau = (1 - 1) / 2 = 0. Score = (0 + 1) / 2 = 0.5
      let prefs = Set.fromList [(optA, optB), (optC, optA)]
          state = setupState testUser1 prefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 0.5

    it "handles ties in Elo ratings (counts as neither concordant nor discordant)" $ do
      -- Prefs: A>C, B>C (total 2)
      -- Elo (ties): A=1600, B=1600, C=1500
      -- Check A>C vs Elo A>C: Concordant (1)
      -- Check B>C vs Elo B>C: Concordant (1)
      -- Tau = (2 - 0) / 2 = 1. Score = (1 + 1) / 2 = 1.0
      let prefs = Set.fromList [(optA, optC), (optB, optC)]
          state = setupState testUser1 prefs Set.empty Set.empty (Set.fromList [optA, optB, optC])
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

      -- Prefs: A>B (total 1)
      -- Elo (ties): A=1600, B=1600
      -- Check A>B vs Elo A=B: Tie (0 conc, 0 disc)
      -- Tau = (0 - 0) / 1 = 0. Score = (0 + 1) / 2 = 0.5
      let prefsAB = Set.singleton (optA, optB)
          stateAB = setupState testUser1 prefsAB Set.empty Set.empty (Set.fromList [optA, optB, optC])
      scoreAB <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultConfig) (Just stateAB)
      scoreAB `shouldBeApprox` 0.5

    it "uses initial rating for options not present in the Elo list" $ do
      -- Prefs: A>D (total 1)
      -- Elo (perfect): A=1700. D is not present, uses initial=1500.
      -- Check A>D vs Elo A(1700) > D(1500): Concordant (1)
      -- Tau = (1 - 0) / 1 = 1. Score = (1+1)/2 = 1.0
      let prefsAD = Set.singleton (optA, optD)
          stateAD = setupState testUser1 prefsAD Set.empty Set.empty (Set.fromList [optA, optD])
      scoreAD <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just stateAD)
      scoreAD `shouldBeApprox` 1.0

      -- Prefs: D>A (total 1)
      -- Elo (perfect): A=1700. D is not present, uses initial=1500.
      -- Check D>A vs Elo A(1700) > D(1500): Discordant (1)
      -- Tau = (0 - 1) / 1 = -1. Score = (-1+1)/2 = 0.0
      let prefsDA = Set.singleton (optD, optA)
          stateDA = setupState testUser1 prefsDA Set.empty Set.empty (Set.fromList [optA, optD])
      scoreDA <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just stateDA)
      scoreDA `shouldBeApprox` 0.0

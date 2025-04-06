{-# LANGUAGE OverloadedStrings #-}

module PreferenceSpec where

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
      emptyUserState options = initialUserState options
      allOptions = allTestOptionsSet
      setupUser user opts uState = mkAppState opts [(user, uState)]

  describe "isPreferred" $ do
    it "returns True if preference exists" $ isPreferred optA optB prefAB `shouldBe` True
    it "returns False if preference does not exist" $ isPreferred optB optA prefAB `shouldBe` False
    it "returns False for unrelated options" $ isPreferred optA optC prefAB `shouldBe` False

  describe "hasBeenCompared" $ do
    it "returns True if A > B exists" $ hasBeenCompared optA optB prefAB `shouldBe` True
    it "returns True if B > A exists" $ hasBeenCompared optB optA prefAB `shouldBe` True
    it "returns False if neither preference exists" $ hasBeenCompared optA optC prefAB `shouldBe` False

  describe "canonicalizeViolation" $ do
    it "correctly canonicalizes A > B, B > C, C > A" $ canonicalizeViolation (optA, optC, optB) `shouldBe` (optA, optC, optB)
    it "correctly canonicalizes B > C, C > A, A > B" $ canonicalizeViolation (optB, optA, optC) `shouldBe` (optA, optC, optB)
    it "correctly canonicalizes C > A, A > B, B > C" $ canonicalizeViolation (optC, optB, optA) `shouldBe` (optA, optC, optB)

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

  let cycleViolationABC = canonicalizeViolation (optA, optC, optB)

  describe "recordComparison" $ do
    it "adds a new preference (Win)" $ do
      let opts = Set.fromList [optA, optB]
          startState = setupUser testUser1 opts (emptyUserState opts)
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      let finalUserState = stateUserStates finalState Map.! testUser1
          prefs = userPreferences finalUserState
      Set.member (optA, optB) prefs `shouldBe` True

    it "removes the canonical pair from uncompared pairs" $ do
      let opts = Set.fromList [optA, optB, optC]
          startState = setupUser testUser1 opts (emptyUserState opts)
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
      let finalUserState = stateUserStates finalState Map.! testUser1
          uncompared = userUncomparedPairs finalUserState
          initialUncompared = userUncomparedPairs (emptyUserState opts)
      Set.member (makeCanonicalPair optA optB) uncompared `shouldBe` False
      Set.size uncompared `shouldBe` (Set.size initialUncompared - 1)

    it "replaces an existing reverse preference" $ do
       let opts = Set.fromList [optA, optB, optC]
           startPrefs = Set.singleton (optB, optA)
           startUncompared = Set.delete (makeCanonicalPair optA optB) (userUncomparedPairs (emptyUserState opts))
           startUserState = (emptyUserState opts) { userPreferences = startPrefs, userUncomparedPairs = startUncompared }
           startState = setupUser testUser1 opts startUserState
       finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)
       let finalUserState = stateUserStates finalState Map.! testUser1
           prefs = userPreferences finalUserState
       Set.member (optA, optB) prefs `shouldBe` True
       Set.member (optB, optA) prefs `shouldBe` False
       Set.size prefs `shouldBe` 1

    it "detects and records a new violation when a comparison completes a cycle" $ do
      let opts3 = Set.fromList [optA, optB, optC]
          startPrefs = Set.fromList [(optB, optC), (optC, optA)]
          startUserState = (emptyUserState opts3)
                              { userPreferences = startPrefs
                              , userViolations = Set.empty
                              }
          startState = setupUser testUser1 opts3 startUserState
          expectedViolation = canonicalizeViolation (optA, optC, optB)

      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing (Just startState)

      let finalUserState = stateUserStates finalState Map.! testUser1
          finalViolations = userViolations finalUserState

      finalViolations `shouldSatisfy` (not . Set.null)
      finalViolations `shouldBe` Set.singleton expectedViolation

    it "removes violations when reversing a preference breaks a cycle" $ do
       let opts3 = Set.fromList [optA, optB, optC]
           startPrefs = Set.fromList [(optA, optB), (optB, optC), (optC, optA)]
           startViolations = Set.singleton cycleViolationABC
           startUserState = (emptyUserState opts3) { userPreferences = startPrefs, userViolations = startViolations }
           startState = setupUser testUser1 opts3 startUserState
       finalState <- execAppTest (recordComparison testUser1 optA optC Win) Nothing (Just startState)
       let finalUserState = stateUserStates finalState Map.! testUser1
           violations = userViolations finalUserState
       Set.null violations `shouldBe` True

  describe "calculateUpdatedViolations" $ do
    let options = Set.fromList [optA, optB, optC]
    it "finds violation in a cyclic preference set" $ do
        let currentPrefs = Set.fromList [(optA, optB), (optB, optC), (optC, optA)]
        let violations = calculateUpdatedViolations options currentPrefs
        Set.null violations `shouldBe` False
        Set.size violations `shouldBe` 1
        Set.findMin violations `shouldBe` canonicalizeViolation (optA, optC, optB)

    it "finds no violations in a consistent preference set" $ do
        let currentPrefs = Set.fromList [(optA, optB), (optB, optC), (optA, optC)]
        let violations = calculateUpdatedViolations options currentPrefs
        Set.null violations `shouldBe` True

  describe "checkIfComplete" $ do
    it "returns True when uncompared and violations are empty" $ do
      let opts3 = Set.fromList [optA, optB, optC]
      let uState = (emptyUserState opts3) { userPreferences = prefsAB_BC, userUncomparedPairs = Set.empty, userViolations = Set.empty }
          state = setupUser testUser1 opts3 uState
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` True
    it "returns False when uncompared is not empty" $ do
      let opts3 = Set.fromList [optA, optB, optC]
      let uState = (emptyUserState opts3) { userPreferences = prefsAB_BC, userUncomparedPairs = Set.singleton (makeCanonicalPair optA optC), userViolations = Set.empty }
          state = setupUser testUser1 opts3 uState
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` False
    it "returns False when violations is not empty" $ do
      let opts3 = Set.fromList [optA, optB, optC]
      let uState = (emptyUserState opts3) { userPreferences = prefsABCycle, userUncomparedPairs = Set.empty, userViolations = Set.singleton cycleViolationABC }
          state = setupUser testUser1 opts3 uState
      complete <- evalAppTest (checkIfComplete testUser1) Nothing (Just state)
      complete `shouldBe` False

  describe "calculateTransitivityScore" $ do
    it "returns 1.0 for fewer than 3 options" $ do
      let opts2 = Set.fromList [optA, optB]
          uState = emptyUserState opts2
          state = mkAppState opts2 [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state)
      score `shouldBeApprox` 1.0

    it "returns 1.0 for 3 options with no violations" $ do
      let opts3 = Set.fromList [optA, optB, optC]
          uState = (emptyUserState opts3) { userPreferences = prefsAB_BC, userViolations = Set.empty }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state)
      score `shouldBeApprox` 1.0

    it "returns 0.0 for 3 options with 1 violation (the only possible triple)" $ do
       let opts3 = Set.fromList [optA, optB, optC]
           uState = (emptyUserState opts3) { userPreferences = prefsABCycle, userViolations = Set.singleton cycleViolationABC }
           state = mkAppState opts3 [(testUser1, uState)]
       score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state)
       score `shouldBeApprox` 0.0

    it "calculates correctly for 4 options with 1 violation" $ do
      let uState = (emptyUserState allOptions) { userViolations = Set.singleton cycleViolationABC }
          state = mkAppState allOptions [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state)
      score `shouldBeApprox` 0.75

    it "calculates correctly for 4 options with 2 violations" $ do
      let cycleViolationABD = canonicalizeViolation (optA, optD, optB)
          violations = Set.fromList [cycleViolationABC, cycleViolationABD]
          uState = (emptyUserState allOptions) { userViolations = violations }
          state = mkAppState allOptions [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing (Just state)
      score `shouldBeApprox` 0.5

  describe "calculateAgreementScore" $ do
    let eloRankPerfect = [(optA, 1700.0), (optB, 1600.0), (optC, 1500.0)]
        eloRankTies = [(optA, 1600.0), (optB, 1600.0), (optC, 1500.0)]
        opts3 = Set.fromList [optA, optB, optC]

    it "returns 1.0 (max agreement) when no preferences exist" $ do
      let uState = emptyUserState opts3
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

    it "returns 1.0 (max agreement) when preferences perfectly match Elo order" $ do
      let prefs = Set.fromList [(optA, optB), (optA, optC), (optB, optC)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

    it "returns 0.0 (max disagreement) when preferences perfectly oppose Elo order" $ do
      let prefs = Set.fromList [(optC, optB), (optC, optA), (optB, optA)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 0.0

    it "returns 0.5 (no correlation) for mixed preferences" $ do
      let prefs = Set.fromList [(optA, optB), (optC, optA)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 0.5

    it "handles ties in Elo ratings (counts as neither concordant nor discordant)" $ do
      let prefs = Set.fromList [(optA, optC), (optB, optC)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultConfig) (Just state)
      score `shouldBeApprox` 1.0

      let prefsAB = Set.singleton (optA, optB)
          uStateAB = (emptyUserState opts3) { userPreferences = prefsAB }
          stateAB = mkAppState opts3 [(testUser1, uStateAB)]
      scoreAB <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultConfig) (Just stateAB)
      scoreAB `shouldBeApprox` 0.5

    it "uses initial rating for options not present in the Elo list" $ do
      let optsAD = Set.fromList [optA, optD]
      let prefsAD = Set.singleton (optA, optD)
          uStateAD = (emptyUserState optsAD) { userPreferences = prefsAD }
          stateAD = mkAppState optsAD [(testUser1, uStateAD)]
      scoreAD <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just stateAD)
      scoreAD `shouldBeApprox` 1.0

      let prefsDA = Set.singleton (optD, optA)
          uStateDA = (emptyUserState optsAD) { userPreferences = prefsDA }
          stateDA = mkAppState optsAD [(testUser1, uStateDA)]
      scoreDA <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultConfig) (Just stateDA)
      scoreDA `shouldBeApprox` 0.0

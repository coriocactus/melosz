{-# LANGUAGE OverloadedStrings #-}

module PreferenceSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types
import AppState
import Preference

import TestUtils

spec :: Spec
spec = describe "Preference" $ do

  let prefAB = Set.singleton (optA, optB) :: Relation
      prefsABCycle = Set.fromList [(optA, optB), (optB, optC), (optC, optA)] :: Relation
      prefsABCLinear = Set.fromList [(optA, optB), (optB, optC), (optA, optC)] :: Relation
      prefsAB_BC = Set.fromList [(optA, optB), (optB, optC)] :: Relation
      allOptions = allTestOptionsSet
      opts3 = Set.fromList [optA, optB, optC]
      cycleViolationABC = canonicalizeViolation (optA, optC, optB)

      setupTestUserState :: Set.Set Option -> Relation -> Set.Set (Option, Option, Option) -> Set.Set (Option, Option) -> UserId -> AppState
      setupTestUserState options prefs viols uncomp user =
          let initialGlicko = Map.fromSet (const initialGlickoPlayer) (Set.map optionId options)
              uState = mkUserState initialGlicko prefs viols uncomp
          in mkAppState options [(user, uState)]

  describe "isPreferred" $ do
    it "returns True if preference exists" $ isPreferred optA optB prefAB `shouldBe` True
    it "returns False if preference does not exist" $ isPreferred optB optA prefAB `shouldBe` False
    it "returns False for unrelated options" $ isPreferred optA optC prefAB `shouldBe` False

  describe "hasBeenCompared" $ do
    it "returns True if A > B exists" $ hasBeenCompared optA optB prefAB `shouldBe` True
    it "returns True if B > A exists" $ hasBeenCompared optB optA prefAB `shouldBe` True -- Check reverse
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

  describe "recordComparison" $ do
    it "adds a new preference (Win)" $ do
      let startState = mkAppState opts3 [(testUser1, simpleUserState opts3)]
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
      Set.member (optA, optB) (userPreferences finalUserState) `shouldBe` True

    it "removes the canonical pair from uncompared pairs" $ do
      let startState = mkAppState opts3 [(testUser1, simpleUserState opts3)]
          initialUncomparedSize = Set.size $ userUncomparedPairs (simpleUserState opts3)
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          uncompared = userUncomparedPairs finalUserState
      Set.member (makeCanonicalPair optA optB) uncompared `shouldBe` False
      Set.size uncompared `shouldBe` (initialUncomparedSize - 1)

    it "replaces an existing reverse preference" $ do
      let startPrefs = Set.singleton (optB, optA)
          startUncompared = Set.delete (makeCanonicalPair optA optB) (getAllOptionPairsSet opts3)
          startState = setupTestUserState opts3 startPrefs Set.empty startUncompared testUser1
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          prefs = userPreferences finalUserState
      Set.member (optA, optB) prefs `shouldBe` True
      Set.member (optB, optA) prefs `shouldBe` False
      Set.size prefs `shouldBe` 1
      userUncomparedPairs finalUserState `shouldBe` startUncompared

    it "detects and records a new violation when a comparison completes a cycle" $ do
      let startPrefs = Set.fromList [(optB, optC), (optC, optA)]
          startUncompared = Set.singleton (makeCanonicalPair optA optB)
          startState = setupTestUserState opts3 startPrefs Set.empty startUncompared testUser1
          expectedViolation = canonicalizeViolation (optA, optC, optB)

      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState

      let finalUserState = stateUserStates finalState Map.! testUser1
          finalViolations = userViolations finalUserState

      finalViolations `shouldBe` Set.singleton expectedViolation
      Set.null (userUncomparedPairs finalUserState) `shouldBe` True

    it "removes violations when reversing a preference breaks a cycle" $ do
      let startPrefs = prefsABCycle
          startViolations = Set.singleton cycleViolationABC
          startUncompared = Set.empty
          startState = setupTestUserState opts3 startPrefs startViolations startUncompared testUser1
      finalState <- execAppTest (recordComparison testUser1 optA optC Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          violations = userViolations finalUserState
          finalPrefs = userPreferences finalUserState
      Set.null violations `shouldBe` True
      finalPrefs `shouldBe` Set.fromList [(optA, optB), (optB, optC), (optA, optC)]

  describe "calculateUpdatedViolations" $ do
    it "finds violation in a cyclic preference set" $ do
        let currentPrefs = prefsABCycle
        let violations = calculateUpdatedViolations opts3 currentPrefs
        Set.size violations `shouldBe` 1
        Set.findMin violations `shouldBe` cycleViolationABC

    it "finds no violations in a consistent preference set" $ do
        let currentPrefs = prefsABCLinear
        let violations = calculateUpdatedViolations opts3 currentPrefs
        Set.null violations `shouldBe` True

  describe "checkIfComplete" $ do
    it "returns True when uncompared and violations are empty" $ do
      let state = setupTestUserState opts3 prefsABCLinear Set.empty Set.empty testUser1
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` True
    it "returns False when uncompared is not empty" $ do
      let uncomp = Set.singleton (makeCanonicalPair optA optC)
          state = setupTestUserState opts3 prefsAB_BC Set.empty uncomp testUser1
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` False
    it "returns False when violations is not empty" $ do
      let state = setupTestUserState opts3 prefsABCycle (Set.singleton cycleViolationABC) Set.empty testUser1
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` False

  describe "calculateTransitivityScore" $ do
    it "returns 1.0 for fewer than 3 options" $ do
      let opts2 = Set.fromList [optA, optB]
          state = mkAppState opts2 [(testUser1, simpleUserState opts2)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 1.0

    it "returns 1.0 for 3 options with no violations" $ do
      let state = setupTestUserState opts3 prefsABCLinear Set.empty Set.empty testUser1
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 1.0

    it "returns 0.0 for 3 options with 1 violation (the only possible triple)" $ do
       let state = setupTestUserState opts3 prefsABCycle (Set.singleton cycleViolationABC) Set.empty testUser1
       score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
       score `shouldBeApprox` 0.0

    it "calculates correctly for 4 options with 1 violation" $ do
      let state = setupTestUserState allOptions Set.empty (Set.singleton cycleViolationABC) Set.empty testUser1
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 0.75

    it "calculates correctly for 4 options with 2 violations" $ do
      let cycleViolationABD = canonicalizeViolation (optA, optD, optB)
          violations = Set.fromList [cycleViolationABC, cycleViolationABD]
          state = setupTestUserState allOptions Set.empty violations Set.empty testUser1
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 0.5

  describe "calculateAgreementScore" $ do
    let setupGlickoState :: Set.Set Option -> Map.Map OptionId Double -> Relation -> UserId -> AppState
        setupGlickoState options ratingsMap prefs user =
          let glickoMap = Map.map (\r -> initialGlickoPlayer { glickoRating = r }) ratingsMap
              uncomp = getAllOptionPairsSet options Set.\\ Set.map (\(w,l) -> makeCanonicalPair w l) prefs
              uState = mkUserState glickoMap prefs Set.empty uncomp
          in mkAppState options [(user, uState)]

    let glickoRatingsPerfect = Map.fromList [(optionId optA, 1700.0), (optionId optB, 1600.0), (optionId optC, 1500.0)]
        glickoRatingsTies = Map.fromList [(optionId optA, 1600.0), (optionId optB, 1600.0), (optionId optC, 1500.0)]

    it "returns 1.0 (max agreement) when no preferences exist" $ do
      let state = setupGlickoState opts3 glickoRatingsPerfect Set.empty testUser1
      score <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

    it "returns 1.0 (max agreement) when preferences perfectly match Glicko rating order" $ do
      let prefs = Set.fromList [(optA, optB), (optA, optC), (optB, optC)]
          state = setupGlickoState opts3 glickoRatingsPerfect prefs testUser1
      score <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

    it "returns 0.0 (max disagreement) when preferences perfectly oppose Glicko rating order" $ do
      let prefs = Set.fromList [(optC, optB), (optC, optA), (optB, optA)]
          state = setupGlickoState opts3 glickoRatingsPerfect prefs testUser1
      score <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) state
      score `shouldBeApprox` 0.0

    it "returns 0.5 (no correlation) for mixed preferences" $ do
      let prefs = Set.fromList [(optA, optB), (optC, optA)]
          state = setupGlickoState opts3 glickoRatingsPerfect prefs testUser1
      score <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) state
      score `shouldBeApprox` 0.5

    it "handles ties in Glicko ratings (counts as neither concordant nor discordant)" $ do
      let prefs = Set.fromList [(optA, optC), (optB, optC)]
          state = setupGlickoState opts3 glickoRatingsTies prefs testUser1
      score <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

      let prefsAB = Set.singleton (optA, optB)
          stateAB = setupGlickoState opts3 glickoRatingsTies prefsAB testUser1
      scoreAB <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) stateAB
      scoreAB `shouldBeApprox` 0.5

    it "uses initial Glicko rating for options not in the user's Glicko map (shouldn't happen with proper init)" $ do
       let optsAD = Set.fromList [optA, optD]
           glickoMap = Map.singleton (optionId optA) 1700.0
           prefsAD = Set.singleton (optA, optD)
           stateAD = setupGlickoState optsAD glickoMap prefsAD testUser1
       scoreAD <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) stateAD
       scoreAD `shouldBeApprox` 1.0

       let prefsDA = Set.singleton (optD, optA)
           stateDA = setupGlickoState optsAD glickoMap prefsDA testUser1
       scoreDA <- evalAppTest (calculateAgreementScore testUser1 []) (Just defaultTestConfig) stateDA
       scoreDA `shouldBeApprox` 0.0

  describe "restoreUserState" $ do
    it "correctly restores preferences and resets Glicko players" $ do
      let opts   = Set.fromList [optA, optB, optC]
          prefs  = Set.fromList [(optA, optB), (optB, optC)]
          initialPlayerMap = Map.fromList
              [ (optionId optA, initialGlickoPlayer { glickoRating = 1600})
              , (optionId optB, initialGlickoPlayer { glickoRating = 1400})
              , (optionId optC, initialGlickoPlayer { glickoRating = 1550})
              ]
          initialUncomp = Set.singleton (makeCanonicalPair optA optC)
          startState = setupTestUserState opts Set.empty Set.empty initialUncomp testUser1
          startStateMod = startState { stateUserStates = Map.adjust (\us -> us { userGlickoPlayers = initialPlayerMap}) testUser1 (stateUserStates startState) }

          expectedUncompared = Set.singleton (makeCanonicalPair optA optC)
          expectedGlicko = Map.fromSet (const initialGlickoPlayer) (Set.map optionId opts)

      finalState <- execAppTest (restoreUserState testUser1 prefs) (Just defaultTestConfig) startStateMod

      case Map.lookup testUser1 (stateUserStates finalState) of
        Nothing -> expectationFailure "UserState not found after restore"
        Just us -> do
          userGlickoPlayers us `shouldBe` expectedGlicko
          userPreferences us `shouldBe` prefs
          userUncomparedPairs us `shouldBe` expectedUncompared
          userViolations us `shouldBe` Set.empty

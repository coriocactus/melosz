{-# LANGUAGE OverloadedStrings #-}

module PreferenceSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad.Reader as MonadReader
import qualified Data.IORef as IORef

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
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          prefs = userPreferences finalUserState
      Set.member (optA, optB) prefs `shouldBe` True

    it "removes the canonical pair from uncompared pairs" $ do
      let opts = Set.fromList [optA, optB, optC]
          startState = setupUser testUser1 opts (emptyUserState opts)
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          uncompared = userUncomparedPairs finalUserState
          initialUncompared = userUncomparedPairs (emptyUserState opts)
      Set.member (makeCanonicalPair optA optB) uncompared `shouldBe` False
      Set.size uncompared `shouldBe` (Set.size initialUncompared - 1)

    it "replaces an existing reverse preference" $ do
      let opts = Set.fromList [optA, optB, optC]
          startPrefs = Set.singleton (optB, optA)
          initialAllPairs = getAllOptionPairsSet opts
          startUncompared = Set.delete (makeCanonicalPair optA optB) initialAllPairs
          startUserState = (emptyUserState opts) { userPreferences = startPrefs, userUncomparedPairs = startUncompared }
          startState = setupUser testUser1 opts startUserState
      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          prefs = userPreferences finalUserState
      Set.member (optA, optB) prefs `shouldBe` True
      Set.member (optB, optA) prefs `shouldBe` False
      Set.size prefs `shouldBe` 1
      userUncomparedPairs finalUserState `shouldBe` startUncompared

    it "detects and records a new violation when a comparison completes a cycle" $ do
      let opts3 = Set.fromList [optA, optB, optC]
          startPrefs = Set.fromList [(optB, optC), (optC, optA)]
          startUncompared = Set.singleton (makeCanonicalPair optA optB)
          startUserState =
            (emptyUserState opts3)
              { userPreferences = startPrefs
              , userViolations = Set.empty
              , userUncomparedPairs = startUncompared
              }
          startState = setupUser testUser1 opts3 startUserState
          expectedViolation = canonicalizeViolation (optA, optC, optB)

      finalState <- execAppTest (recordComparison testUser1 optA optB Win) Nothing startState

      let finalUserState = stateUserStates finalState Map.! testUser1
          finalViolations = userViolations finalUserState

      finalViolations `shouldSatisfy` (not . Set.null)
      finalViolations `shouldBe` Set.singleton expectedViolation
      Set.null (userUncomparedPairs finalUserState) `shouldBe` True

    it "removes violations when reversing a preference breaks a cycle" $ do
      let opts3 = Set.fromList [optA, optB, optC]
          startPrefs = Set.fromList [(optA, optB), (optB, optC), (optC, optA)]
          startViolations = Set.singleton cycleViolationABC
          startUncompared = Set.empty
          startUserState =
            (emptyUserState opts3)
              { userPreferences = startPrefs
              , userViolations = startViolations
              , userUncomparedPairs = startUncompared
              }
          startState = setupUser testUser1 opts3 startUserState
      finalState <- execAppTest (recordComparison testUser1 optA optC Win) Nothing startState
      let finalUserState = stateUserStates finalState Map.! testUser1
          violations = userViolations finalUserState
          finalPrefs = userPreferences finalUserState
      Set.null violations `shouldBe` True
      finalPrefs `shouldBe` Set.fromList [(optA, optB), (optB, optC), (optA, optC)]


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
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` True
    it "returns False when uncompared is not empty" $ do
      let opts3 = Set.fromList [optA, optB, optC]
      let uState = (emptyUserState opts3) { userPreferences = prefsAB_BC, userUncomparedPairs = Set.singleton (makeCanonicalPair optA optC), userViolations = Set.empty }
          state = setupUser testUser1 opts3 uState
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` False
    it "returns False when violations is not empty" $ do
      let opts3 = Set.fromList [optA, optB, optC]
      let uState = (emptyUserState opts3) { userPreferences = prefsABCycle, userUncomparedPairs = Set.empty, userViolations = Set.singleton cycleViolationABC }
          state = setupUser testUser1 opts3 uState
      complete <- evalAppTest (checkIfComplete testUser1) Nothing state
      complete `shouldBe` False

  describe "calculateTransitivityScore" $ do
    it "returns 1.0 for fewer than 3 options" $ do
      let opts2 = Set.fromList [optA, optB]
          uState = emptyUserState opts2
          state = mkAppState opts2 [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 1.0

    it "returns 1.0 for 3 options with no violations" $ do
      let opts3 = Set.fromList [optA, optB, optC]
          uState = (emptyUserState opts3) { userPreferences = prefsAB_BC, userViolations = Set.empty }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 1.0

    it "returns 0.0 for 3 options with 1 violation (the only possible triple)" $ do
       let opts3 = Set.fromList [optA, optB, optC]
           uState = (emptyUserState opts3) { userPreferences = prefsABCycle, userViolations = Set.singleton cycleViolationABC }
           state = mkAppState opts3 [(testUser1, uState)]
       score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
       score `shouldBeApprox` 0.0

    it "calculates correctly for 4 options with 1 violation" $ do
      let uState = (emptyUserState allOptions) { userViolations = Set.singleton cycleViolationABC }
          state = mkAppState allOptions [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 0.75 -- 1 - 1/C(4,3) = 1 - 1/4 = 0.75

    it "calculates correctly for 4 options with 2 violations" $ do
      let cycleViolationABD = canonicalizeViolation (optA, optD, optB) -- Assuming A > B > D > A
          violations = Set.fromList [cycleViolationABC, cycleViolationABD]
          uState = (emptyUserState allOptions) { userViolations = violations }
          state = mkAppState allOptions [(testUser1, uState)]
      score <- evalAppTest (calculateTransitivityScore testUser1) Nothing state
      score `shouldBeApprox` 0.5 -- 1 - 2/C(4,3) = 1 - 2/4 = 0.5

  describe "calculateAgreementScore" $ do
    let eloRankPerfect = [(optA, 1700.0), (optB, 1600.0), (optC, 1500.0)]
        eloRankTies = [(optA, 1600.0), (optB, 1600.0), (optC, 1500.0)]
        opts3 = Set.fromList [optA, optB, optC]

    it "returns 1.0 (max agreement) when no preferences exist" $ do
      let uState = emptyUserState opts3
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

    it "returns 1.0 (max agreement) when preferences perfectly match Elo order" $ do
      let prefs = Set.fromList [(optA, optB), (optA, optC), (optB, optC)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

    it "returns 0.0 (max disagreement) when preferences perfectly oppose Elo order" $ do
      let prefs = Set.fromList [(optC, optB), (optC, optA), (optB, optA)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) state
      score `shouldBeApprox` 0.0

    it "returns 0.5 (no correlation) for mixed preferences" $ do
      -- A > B (concordant), C > A (discordant)
      let prefs = Set.fromList [(optA, optB), (optC, optA)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      -- Concordant = 1, Discordant = 1, Total = 2
      -- Tau = (1 - 1) / 2 = 0
      -- Score = (0 + 1) / 2 = 0.5
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) state
      score `shouldBeApprox` 0.5

    it "handles ties in Elo ratings (counts as neither concordant nor discordant)" $ do
      -- Elo: A=1600, B=1600, C=1500
      -- Prefs: A > C (concordant), B > C (concordant)
      let prefs = Set.fromList [(optA, optC), (optB, optC)]
          uState = (emptyUserState opts3) { userPreferences = prefs }
          state = mkAppState opts3 [(testUser1, uState)]
      -- Concordant = 2, Discordant = 0, Total = 2
      -- Tau = (2 - 0) / 2 = 1
      -- Score = (1 + 1) / 2 = 1.0
      score <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultTestConfig) state
      score `shouldBeApprox` 1.0

      -- Prefs: A > B (tie in elo, ignored)
      let prefsAB = Set.singleton (optA, optB)
          uStateAB = (emptyUserState opts3) { userPreferences = prefsAB }
          stateAB = mkAppState opts3 [(testUser1, uStateAB)]
      -- Concordant = 0, Discordant = 0, Total = 1
      -- Tau = (0 - 0) / 1 = 0
      -- Score = (0 + 1) / 2 = 0.5
      scoreAB <- evalAppTest (calculateAgreementScore testUser1 eloRankTies) (Just defaultTestConfig) stateAB
      scoreAB `shouldBeApprox` 0.5

    it "uses initial rating for options not present in the Elo list" $ do
      let optsAD = Set.fromList [optA, optD]
      -- Elo: A=1700. D uses initial=1500.
      -- Prefs: A > D (concordant, since 1700 > 1500)
      let prefsAD = Set.singleton (optA, optD)
          uStateAD = (emptyUserState optsAD) { userPreferences = prefsAD }
          stateAD = mkAppState optsAD [(testUser1, uStateAD)]
      -- Concordant = 1, Discordant = 0, Total = 1. Score = 1.0
      scoreAD <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) stateAD
      scoreAD `shouldBeApprox` 1.0

      -- Prefs: D > A (discordant, since 1500 < 1700)
      let prefsDA = Set.singleton (optD, optA)
          uStateDA = (emptyUserState optsAD) { userPreferences = prefsDA }
          stateDA = mkAppState optsAD [(testUser1, uStateDA)]
      -- Concordant = 0, Discordant = 1, Total = 1. Score = 0.0
      scoreDA <- evalAppTest (calculateAgreementScore testUser1 eloRankPerfect) (Just defaultTestConfig) stateDA
      scoreDA `shouldBeApprox` 0.0

  describe "restoreUserState" $ do
    it "correctly restores user state from ratings and preferences" $ do
      let red    = createOption "red" "Red" ""
          blue   = createOption "blue" "Blue" ""
          green  = createOption "green" "Green" ""
          opts   = Set.fromList [red, blue, green]
          prefs  = Set.fromList [(red, blue), (blue, green)]
          ratings = Map.fromList [(optionId red, 1600), (optionId blue, 1500), (optionId green, 1400)]
          uid    = "user-a"
          expectedUncompared = Set.fromList [makeCanonicalPair red green]

      stateRef <- IORef.newIORef initialState
      let config = AppConfig 32.0 1500.0 stateRef

      flip MonadReader.runReaderT config $ do
        modifyStateRef_ $ \s -> s { stateOptions = opts }

        restoreUserState uid prefs ratings
        mUserState <- getUserState uid

        case mUserState of
          Nothing -> MonadReader.liftIO $ expectationFailure "UserState not found"
          Just us -> do
            let actualRatings         = userRatings us
                actualPreferences     = userPreferences us
                actualUncomparedPairs = userUncomparedPairs us
                actualViolations      = userViolations us

            MonadReader.liftIO $ actualRatings `shouldBe` ratings
            MonadReader.liftIO $ actualPreferences `shouldBe` prefs
            MonadReader.liftIO $ actualUncomparedPairs `shouldBe` expectedUncompared
            MonadReader.liftIO $ actualViolations `shouldBe` Set.empty

module Marshal where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types
import AppState

setupOption :: Option -> App ()
setupOption newOption = do
  currentState <- readCurrentState
  let alreadyExists = Set.member newOption (stateOptions currentState)
  Monad.unless alreadyExists $ do
    let existingOptions = stateOptions currentState
        pairsToAdd = Set.map (makeCanonicalPair newOption) (Set.delete newOption existingOptions)

    modifyStateRef_ $ \s ->
      let updatedOptions = Set.insert newOption (stateOptions s)
          updateUserState _ us = us { userUncomparedPairs = Set.union pairsToAdd (userUncomparedPairs us) }
          updatedUserStates = Map.mapWithKey updateUserState (stateUserStates s)
      in s { stateOptions = updatedOptions, stateUserStates = updatedUserStates }

setupUser :: UserId -> App ()
setupUser userId = do
  currentState <- readCurrentState
  let userExists = Map.member userId (stateUserStates currentState)
  Monad.unless userExists $ do
    optionsSet <- getOptions
    let newUserState = initialUserState optionsSet
    modifyStateRef_ $ \s -> s
      { stateUserStates = Map.insert userId newUserState (stateUserStates s)
      }

setupOptions :: [Option] -> App ()
setupOptions opts = Monad.forM_ opts setupOption

setupUsers :: [UserId] -> App ()
setupUsers users = Monad.forM_ users setupUser

module Marshal where

import qualified Control.Monad as Monad
import qualified Control.Monad.State as MonadState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types
import AppState

setupOption :: Option -> App ()
setupOption newOption = do
  alreadyExists <- MonadState.gets (Set.member newOption . stateOptions)
  Monad.unless alreadyExists $ do
    MonadState.modify $ \s -> s { stateOptions = Set.insert newOption (stateOptions s) }

    existingOptions <- MonadState.gets stateOptions
    let pairsToAdd = Set.map (makeCanonicalPair newOption) (Set.delete newOption existingOptions)

    MonadState.modify $ \s ->
      let updateUserState _ us = us { userUncomparedPairs = Set.union pairsToAdd (userUncomparedPairs us) }
      in s { stateUserStates = Map.mapWithKey updateUserState (stateUserStates s) }

setupUser :: UserId -> App ()
setupUser userId = do
  userExists <- MonadState.gets (Map.member userId . stateUserStates)
  Monad.unless userExists $ do
    optionsSet <- getOptions
    let newUserState = initialUserState optionsSet
    MonadState.modify $ \s -> s
      { stateUserStates = Map.insert userId newUserState (stateUserStates s)
      }

setupOptions :: [Option] -> App ()
setupOptions opts = Monad.forM_ opts setupOption

setupUsers :: [UserId] -> App ()
setupUsers users = Monad.forM_ users setupUser

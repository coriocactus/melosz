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
    existingOptions <- MonadState.gets stateOptions -- Get options *after* adding new one
    userIds <- getUsers
    let pairsToAdd = Set.map (makeCanonicalPair newOption) (Set.delete newOption existingOptions) -- Pairs involving the new option

    MonadState.modify $ \s ->
      let updateUncompared u currentMap =
            Map.insertWith Set.union u pairsToAdd currentMap
      in s { stateUncomparedPairs = Set.foldr updateUncompared (stateUncomparedPairs s) userIds }

setupUser :: UserId -> App ()
setupUser userId = do
  alreadyExists <- MonadState.gets (Set.member userId . stateUsers)
  Monad.unless alreadyExists $ do
    optionsSet <- getOptions
    let initialUncompared = getAllOptionPairsSet optionsSet
    MonadState.modify $ \s -> s
      { stateUsers = Set.insert userId (stateUsers s)
      , statePreferences = Map.insert userId Set.empty (statePreferences s)
      , stateViolations = Map.insert userId Set.empty (stateViolations s)
      , stateUncomparedPairs = Map.insert userId initialUncompared (stateUncomparedPairs s)
      }

setupOptions :: [Option] -> App ()
setupOptions opts = Monad.forM_ opts setupOption

setupUsers :: [UserId] -> App ()
setupUsers users = Monad.forM_ users setupUser

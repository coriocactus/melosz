module Marshal where

import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as MonadReader
import qualified Data.Map.Strict as Map

import Types
import AppState

setupUser :: UserId -> App ()
setupUser userId = do
  currentState <- readCurrentState
  let userExists = Map.member userId (stateUserStates currentState)
  Monad.unless userExists $ do
    optionsSet <- MonadReader.asks configOptions
    let newUserState = initialUserState optionsSet
    modifyStateRef_ $ \s -> s
      { stateUserStates = Map.insert userId newUserState (stateUserStates s)
      }

setupUsers :: [UserId] -> App ()
setupUsers users = Monad.forM_ users setupUser

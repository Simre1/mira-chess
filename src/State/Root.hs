module State.Root where

import Data.Database
import State.Model
import Control.FunctorMap
import qualified Data.Map.Strict as M
import Data.StateControl
import Data.Chess
import Data.Id
import State.Tab

data RootState f = RootState
  { database :: Database
  , model :: Model f
  }

instance FunctorMap RootState where
  functorMap f state@(RootState {model}) = state {model = functorMap f model}

initializeRootState :: IO (RootState StateControl)
initializeRootState = do
  database <- createDatabase "database"
  g <- newStateControl newGame
  tabs <- Tabs <$> (newStateControl M.empty) <*> (newStateControl (Id (-1))) <*> (newStateControl [])
  pure (RootState database (Model tabs))

closeAppState :: RootState StateControl -> IO ()
closeAppState appState = closeDatabase $ database appState
module AppState where

import ReactiveMarkup.SimpleEvents

import Data.StateControl
import Data.Database
import Data.Id
import qualified Data.Map as M
import Data.Chess
import Data.FunctorMap
import Data.Empty

data AppState f = AppState
  { database :: Database
  , model :: Model f
  }

data Model f = Model
  { chessGames :: f (M.Map (Id ChessGame) (f ChessGame))
  , tabs :: f (M.Map (Id (Tab Empty)) (Tab f))
  }

data Tab f = Tab
  { tabId :: Id (Tab Empty)
  }

instance FunctorMap Model where
  functorMap f (Model games tabs) = Model (fmap f <$> f games) (fmap (functorMap f) <$> f tabs)

instance FunctorMap AppState where
  functorMap f state@(AppState {model}) = state {model = functorMap f model}

instance FunctorMap Tab where
  functorMap _ (Tab i) = Tab i

initializeAppState :: IO (AppState StateControl)
initializeAppState = do
  games <- newStateControl M.empty
  database <- createDatabase "database"
  g <- newStateControl newGame
  tabs <- newStateControl M.empty
  modifyStateControl games (M.insert (Id 0) g)
  pure (AppState database (Model games tabs))

closeAppState :: AppState Dynamic -> IO ()
closeAppState appState = closeDatabase $ database appState
module AppState where

import ReactiveMarkup.SimpleEvents

import Data.Chess
import Data.IdStore
import Data.FunctorMap
import Data.Database
import Optics.Core
import Database.Selda (def)

data AppState f = AppState
  { database' :: Database
  , model' :: Model f
  }

data Model f = Model
  { chessGames' :: IdStore ChessGame f
  }

instance FunctorMap AppState where
  functorMap morph appState@(AppState {model'}) = appState {model' = functorMap morph model'}

instance FunctorMap Model where
  functorMap morph (Model cp) = Model $ functorMap morph cp

chessGames :: Getter (Model f) (IdStore ChessGame f)
chessGames = to chessGames'

model :: Lens' (AppState f) (Model f)
model = lens getter setter
  where getter (AppState {model'}) = model'
        setter state mod = state {model' = mod}

database :: Lens' (AppState f) Database
database = lens getter setter
  where getter (AppState {database'}) = database'
        setter appState db = appState {database' = db}

initializeAppState :: IO (AppState Dynamic, Model EventTrigger)
initializeAppState = do
  (chessPositionsD, chessPositionsT) <- newIdStore newGame
  database <- createDatabase "database"
  games <- runDatabaseM database $ do
    insertChessGames [DBChessGame def "Greetings"]
    getChessGame
  print games
  pure (AppState database $ Model chessPositionsD, Model chessPositionsT)

closeAppState :: AppState Dynamic -> IO ()
closeAppState appState = closeDatabase $ appState ^. database
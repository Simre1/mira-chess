module AppState where

import ReactiveMarkup.SimpleEvents
import Data.Maybe

import UI.MyComponents.ChessBoard

import qualified Game.Chess as C

import qualified Data.Map as M

import AppState.Chess

newtype Id = Id Int deriving (Eq, Ord, Show)

data AppState f = AppState 
  { chessPositions :: f (M.Map Id ChessPosition)
  }

initialAppState :: IO (AppState Dynamic, AppState EventTrigger)
initialAppState = do
  (chessPositionsD, chessPositionsT) <- newDynamic $ M.insert (Id 0) (ChessPosition C.startpos) M.empty
  pure (AppState chessPositionsD, AppState chessPositionsT)

getChessPosition :: Id -> AppState Dynamic -> Dynamic ChessPosition
getChessPosition id' state = fromJust . M.lookup id' <$> chessPositions state

module AppState where

import ReactiveMarkup.SimpleEvents

import Data.Chess
import Data.IdStore
import Data.FunctorMap
import Optics.Core

data AppState f = AppState
  { chessPositions' :: IdStore ChessPosition f
  }

instance FunctorMap AppState where
  functorMap morph (AppState chessPositions) = AppState $ functorMap morph chessPositions

chessPositions :: Getter (AppState f) (IdStore ChessPosition f)
chessPositions = to chessPositions'

initialAppState :: IO (AppState Dynamic, AppState EventTrigger)
initialAppState = do
  (chessPositionsD, chessPositionsT) <- newIdStore startPosition
  pure (AppState chessPositionsD, AppState chessPositionsT)

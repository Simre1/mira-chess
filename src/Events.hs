module Events where

import qualified Data.Text as T
import Data.Chess
import AppState
import Data.IdStore

import View.Components.ChessBoard (ChessBoardEvent)

data AppEvent
  = Log T.Text
  | ChessBoardEvent Id ChessBoardEvent
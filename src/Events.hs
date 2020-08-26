module Events where

import qualified Data.Text as T
import Data.Chess
import AppState
import Data.Id

import View.Components.ChessBoard (ChessBoardEvent)

data AppEvent
  = Log T.Text
  | ChessBoardEvent (Id ChessGame) ChessBoardEvent
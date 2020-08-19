module Handler where

import ReactiveMarkup.SimpleEvents

import qualified Data.Map as M
import Optics.Core

import Data.Chess
import Data.IdStore
import AppState
import Events

import View.Components.ChessBoard (ChessBoardEvent(..))

handleEvents :: AppState Behavior -> Model EventTrigger -> AppEvent -> IO ()
handleEvents appState triggers event = 
  case event of
    Log t -> print t
    ChessBoardEvent chessId chessBoardEvent -> do
      chessGame <- current $ appState ^. model % chessGames % lookupId chessId
      case chessBoardEvent of
        MakeMove move ->
          triggerEvent (triggers ^. chessGames %% lookupId chessId) $ focusNextMove $ insertMainMoveAtCurrent move chessGame
        NextMove ->
          triggerEvent (triggers ^. chessGames %% lookupId chessId) $ focusNextMove chessGame
        PreviousMove ->
          triggerEvent (triggers ^. chessGames %% lookupId chessId) $ focusPreviousMove chessGame
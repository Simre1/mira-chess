module Handler where

import ReactiveMarkup.SimpleEvents

import qualified Data.Map as M
import Optics.Core

import Data.Chess
import Data.StateControl
import AppState
import Events



import View.Components.ChessBoard (ChessBoardEvent(..))
import Data.Maybe (fromMaybe)

handleEvents :: AppState StateControl -> AppEvent -> IO ()
handleEvents appState event = 
  case event of
    Log t -> print t
    ChessBoardEvent chessId chessBoardEvent -> do
      maybeChessGame <- fmap (M.lookup chessId). getStateControl . chessGames . model $ appState 
      let handleEvent controlChessGame = do
            case chessBoardEvent of
              MakeMove move ->
                modifyStateControl controlChessGame $ \chessGame -> fromMaybe chessGame $ (focusNextMove $ insertMainMoveAtCurrent move chessGame)
              NextMove ->
                modifyStateControl controlChessGame $ \chessGame -> fromMaybe chessGame $ focusNextMove chessGame
              PreviousMove -> do
                modifyStateControl controlChessGame $ \chessGame -> fromMaybe chessGame $ focusPreviousMove chessGame
      maybe mempty handleEvent maybeChessGame
module Handler where

import ReactiveMarkup.SimpleEvents

import qualified Data.Map as M
import Optics.Core

import Data.Chess
import Data.IdStore
import AppState
import Events

handleEvents :: AppState Behavior -> AppState EventTrigger -> AppEvent -> IO ()
handleEvents appState triggers event = 
  case event of
    Log t -> print t
    DoMove chessId move -> do
      chessPosition <- current $ appState ^. chessPositions %% lookupId chessId
      triggerEvent (triggers ^. chessPositions %% lookupId chessId) $ executeMove move chessPosition
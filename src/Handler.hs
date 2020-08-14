module Handler where

import ReactiveMarkup.SimpleEvents

import qualified Data.Map as M

import AppState.Chess
import AppState
import Events

handleEvents :: AppState Dynamic -> AppState EventTrigger -> AppEvent -> IO ()
handleEvents appState triggers event = 
  case event of
    Log t -> print t
    DoMove chessId move -> do
       currentPositions <- current $ toBehavior $ chessPositions appState
       triggerEvent (chessPositions triggers) $ M.alter (fmap (executeMove move)) chessId currentPositions
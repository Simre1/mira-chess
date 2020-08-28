module Handler where

import ReactiveMarkup.SimpleEvents

import qualified Data.Map as M
import Data.Chess
import Data.StateControl
import Events

import State.Root
import State.Model
import State.Tab
import Data.Id

import Data.Maybe (fromMaybe)

handleEvents :: RootState StateControl -> RootEvent -> IO ()
handleEvents appState event = 
  case event of
    Log t -> print t
   
    CreateTab name -> do
      let controlTabs = tabs . model $ appState
      i <- modifyStateControlIO' (tabMap controlTabs) $ \tabMap -> do
        let i = M.size tabMap
        tab <- Tab (Id i) name <$> newStateControl newGame
        pure (M.insert (Id i) tab tabMap, Id i)
      putStateControl (activeTab controlTabs) i
      modifyStateControl (tabOrder controlTabs) (<> [i])
    TabEvent tabId event -> do 
      maybeTab <- fmap (M.lookup tabId). getStateControl . tabMap . tabs . model $ appState 
      let handleEvent tab = case event of
            ChessGameEvent chessBoardEvent -> do
              case chessBoardEvent of
                MakeMove move ->
                  modifyStateControl (tabChessGame tab) $ \chessGame -> fromMaybe chessGame $ (focusNextMove $ insertMainMoveAtCurrent move chessGame)
                NextMove ->
                  modifyStateControl (tabChessGame tab) $ \chessGame -> fromMaybe chessGame $ focusNextMove chessGame
                PreviousMove -> do
                  modifyStateControl (tabChessGame tab) $ \chessGame -> fromMaybe chessGame $ focusPreviousMove chessGame
      maybe mempty handleEvent maybeTab
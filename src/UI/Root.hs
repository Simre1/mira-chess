module UI.Root where 

import ReactiveMarkup
import AppState
import Events
import UI.MyMarkup
import UI.MyComponents.ChessBoard

root :: AppState Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root appState = expandMarkup $ 
  window none $ dynamicMarkup (getChessPosition (ChessPositionId 0) appState) $ \chessPosition ->
    chessBoard (chessPositionToPositionData chessPosition)
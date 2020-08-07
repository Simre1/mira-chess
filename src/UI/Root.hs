module UI.Root where 

import ReactiveMarkup
import AppState
import Events
import UI.MyMarkup
import UI.MyComponents.ChessBoard
import qualified Data.Text as T


root :: AppState Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root appState = expandMarkup $ 
  window none $ dynamicMarkup (getChessPosition (Id 0) appState) $ \chessPosition ->
    DoMove (Id 0) <$> chessBoard (chessPositionToPositionData chessPosition)
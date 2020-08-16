module View.Root where 

import ReactiveMarkup
import AppState
import Events
import View.Markup
import View.Components.ChessBoard
import qualified Data.Text as T

root :: AppState Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root appState = expandMarkup $ 
  window none $
    DoMove (Id 0) <$> chessBoard (getChessPosition (Id 0) appState)
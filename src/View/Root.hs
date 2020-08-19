module View.Root where 

import ReactiveMarkup hiding ((%%))
import AppState
import Events
import View.Markup
import View.Components.ChessBoard
import qualified Data.Text as T
import Optics.Core
import Data.IdStore

root :: Model Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root appState = expandMarkup $ 
  window none $
    ChessBoardEvent (Id 0) <$> chessBoard ((appState ^. chessGames %% lookupId (Id 0)))
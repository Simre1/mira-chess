module View.Root where 

import ReactiveMarkup hiding ((%%))
import AppState
import Events
import View.Markup
import View.Components.ChessBoard
import qualified Data.Text as T
import Optics.Core
import Data.IdStore

root :: AppState Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root appState = expandMarkup $ 
  window none $
    DoMove (Id 0) <$> chessBoard ((appState ^. chessPositions %% lookupId (Id 0)))
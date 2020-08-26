module View.Root where 

import ReactiveMarkup hiding ((%%))
import AppState
import Events
import View.Markup
import View.Components.ChessBoard
import qualified Data.Text as T
import Optics.Core
import Data.Maybe (fromMaybe)
import Data.Chess (newGame)

import Data.Id
import qualified Data.Map as M

root :: Model Dynamic -> Markup '[Window '[Text]] MyMarkup AppEvent
root model = expandMarkup $ 
  window none $
    ChessBoardEvent (Id 0) <$> chessBoard (switchDynamics $ fromMaybe (pure newGame) . M.lookup (Id 0) <$> chessGames model)
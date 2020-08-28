module View.Root where 

import ReactiveMarkup

import State.Model
import View.Markup
import Events
import qualified Data.Map.Strict as M
import Data.Id
import Data.Chess
import Data.Maybe
import Control.FunctorMap
import View.Components.ChessBoard
import State.Tab

root :: Model Dynamic -> Markup '[Window '[Text]] MyMarkup RootEvent
root model = 
  window (text "MiraChess") $
    tabNotebook model

tabNotebook :: Model Dynamic -> SimpleMarkup MyMarkup RootEvent
tabNotebook model = expandMarkup $
  dynamicMarkup (tabMap (tabs model)) $ \tabMap -> notebook $
    M.foldrWithKey (\k tabState others -> tab tabState : others) [] tabMap
  where 
    tab :: Tab Dynamic -> (Markup '[Label '[Text]] '[] RootEvent, SimpleMarkup MyMarkup RootEvent)
    tab tab = (label (text $ tabName tab), expandMarkup content)
      where 
        content = 
          fmap (TabEvent (tabId tab) . ChessGameEvent) $ 
            chessBoard (tabChessGame tab)

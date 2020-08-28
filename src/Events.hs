module Events where

import qualified Data.Text as T
import Data.Chess
import Data.Id
import State.Tab
import Data.Typeable

data RootEvent
  = Log T.Text
  | CreateTab T.Text
  | TabEvent (IdF Tab) TabEvent

data ChessGameEvent
  = MakeMove Move
  | PreviousMove
  | NextMove

data TabEvent
  = SelectTab (IdF Tab)
  | ChessGameEvent ChessGameEvent
module Events where

import qualified Data.Text as T
import AppState.Chess
import AppState

data AppEvent
  = Log T.Text
  | DoMove Id Move
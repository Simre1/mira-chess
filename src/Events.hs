module Events where

import qualified Data.Text as T
import Data.Chess
import AppState
import Data.IdStore

data AppEvent
  = Log T.Text
  | DoMove Id Move
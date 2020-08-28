module State.Model where

import qualified Data.Map.Strict as M

import Data.Id
import Data.Chess
import Control.FunctorMap
import Data.Empty

import State.Tab

data Model (f :: * -> *) = Model
  { tabs :: Tabs f
  }

instance FunctorMap Model where
  functorMap f (Model tabs) = Model (functorMap f tabs)

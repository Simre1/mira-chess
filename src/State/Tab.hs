module State.Tab where

import Control.FunctorMap
import Data.Chess
import Data.Id
import qualified Data.Text as T
import Data.Coerce
import qualified Data.Map.Strict as M

data Tabs f = Tabs
  { tabMap :: f (M.Map (IdF Tab) (Tab f))
  , activeTab :: f (IdF Tab)
  , tabOrder :: f [IdF Tab]
  }

data Tab f = Tab
  { tabId :: IdF Tab,
    tabName :: T.Text,
    tabChessGame :: f ChessGame
  }

instance FunctorMap Tabs where
  functorMap f (Tabs m a o) = Tabs (fmap (functorMap f) <$> f m) (f a) (f o)

instance FunctorMap Tab where
  functorMap f (Tab i n c) = Tab i n $ f c

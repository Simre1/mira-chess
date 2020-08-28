module Data.Id where

import Data.Empty

newtype Id (a :: *) = Id Int deriving (Eq, Ord, Show)

type IdF f = Id (f Empty)
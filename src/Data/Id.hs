module Data.Id where

newtype Id a = Id Int deriving (Eq, Ord, Show)
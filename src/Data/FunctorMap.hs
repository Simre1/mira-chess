module Data.FunctorMap where

class FunctorMap t where
  functorMap :: (forall x. f x -> g x) -> t f -> t g
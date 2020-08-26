module Data.FunctorMap where

class FunctorMap t where
  functorMap :: Functor g => (forall x. f x -> g x) -> t f -> t g
module Data.IdStore where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.FunctorMap
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import ReactiveMarkup.SimpleEvents
import Optics.Core

newtype Id = Id {idInt :: Int} deriving (Eq, Ord, Show)

data IdStore x f = IdStore {lookupId' :: Id -> f x}

instance FunctorMap (IdStore x) where
  functorMap morph (IdStore f) = IdStore $ morph . f

lookupId :: Id -> Getter (IdStore x f) (f x)
lookupId i = to $ \store -> lookupId' store i

newIdStore :: x -> IO (IdStore x Dynamic, IdStore x EventTrigger)
newIdStore def = do
  (dyn, trigger) <- newDynamic (Id (-1), id)
  let triggerVal = IdStore $ \i -> contramap (\x -> (i, M.insert i x)) $ trigger
      dynMap = (\(i, f) -> (i, f M.empty)) <$> dyn
      dynVal = IdStore $ \i ->
        customDynamic
          (fmap (fromMaybe def . M.lookup i . snd) . toBehavior $ dynMap)
          (fmap (fromMaybe def . M.lookup i . snd) . filterE (\(x, _) -> x == i) . toEvent $ dynMap)
  pure $ (dynVal, triggerVal)

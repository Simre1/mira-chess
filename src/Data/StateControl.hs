module Data.StateControl where

import ReactiveMarkup.SimpleEvents

data StateControl a = StateControl { stateControlToDynamic :: Dynamic a, stateControlToTrigger :: EventTrigger a}

newStateControl :: a -> IO (StateControl a)
newStateControl = fmap (uncurry StateControl) . newDynamic

putStateControl :: StateControl a -> a -> IO ()
putStateControl (StateControl _ t) a = triggerEvent t a

modifyStateControl :: StateControl a -> (a -> a) -> IO ()
modifyStateControl stateControl f = do
  v <- getStateControl stateControl
  putStateControl stateControl $ f v

getStateControl :: StateControl a -> IO a
getStateControl (StateControl d _) = current $ toBehavior d

bindStateControl :: (a -> StateControl b) -> StateControl a -> StateControl b
bindStateControl f (StateControl d t) = StateControl 
  (switchDynamics $ stateControlToDynamic <$> dynamicStateControl) 
  (subsumeIO $ do
    StateControl _ t <- current $ toBehavior dynamicStateControl
    pure t  
  )
  where 
    dynamicStateControl = f <$> d
    subsumeIO ioTrigger = EventTrigger $ \a -> ioTrigger >>= flip triggerEvent a

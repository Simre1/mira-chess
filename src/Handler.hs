module Handler where

import ReactiveMarkup.SimpleEvents

import AppState
import Events

handleEvents :: AppState Dynamic -> AppState EventTrigger -> AppEvent -> IO ()
handleEvents _ _ _ = pure ()
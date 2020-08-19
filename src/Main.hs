module Main where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import AppState
import Handler
import View.Root
import View.Runner

import qualified GI.Gtk as Gtk
import Control.Monad.IO.Class (MonadIO(liftIO))
import HotReload (hotReloadMarkupWithoutAsking)
import Data.FunctorMap
import Optics.Core

main :: IO ()
main = initializeChessApp >>= runGtkWidget

initializeChessApp :: IO (GtkM Gtk.Widget)
initializeChessApp = do
  (appState, modelTriggers) <- initializeAppState
  runner <- makeRunner
  let handle = 
        handleEvents (functorMap toBehavior appState) modelTriggers
  pure $ runMarkupWithTwo windowRunner runner handle (root $ appState ^. model)

hotReload :: IO ()
hotReload = initializeChessApp >>= hotReloadMarkupWithoutAsking
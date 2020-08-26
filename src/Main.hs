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
import Data.StateControl

main :: IO ()
main = initializeChessApp >>= runGtkWidget

initializeChessApp :: IO (GtkM Gtk.Widget)
initializeChessApp = do
  controlAppState <- initializeAppState
  runner <- makeRunner
  let handle = 
        handleEvents (controlAppState)
  pure $ runMarkupWithTwo windowRunner runner handle (root $ functorMap stateControlToDynamic $ model controlAppState)

hotReload :: IO ()
hotReload = initializeChessApp >>= hotReloadMarkupWithoutAsking
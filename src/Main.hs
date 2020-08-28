module Main where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import Handler
import View.Root
import View.Runner

import qualified GI.Gtk as Gtk
import Control.Monad.IO.Class (MonadIO(liftIO))
import HotReload (hotReloadMarkupWithoutAsking)
import Control.FunctorMap
import Optics.Core
import Data.StateControl
import Events

import State.Root

main :: IO ()
main = initializeChessApp >>= runGtkWidget

initializeChessApp :: IO (GtkM Gtk.Widget)
initializeChessApp = do
  rootState <- initializeRootState
  runner <- makeRunner
  let handle = 
        handleEvents rootState
  handle $ CreateTab "My cool tab"
  handle $ CreateTab "Second tab"
  pure $ runMarkupWithTwo windowRunner runner handle (root $ functorMap stateControlToDynamic $ model rootState)

hotReload :: IO ()
hotReload = initializeChessApp >>= hotReloadMarkupWithoutAsking
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

main :: IO ()
main = initializeChessApp >>= runGtkWidget

initializeChessApp :: IO (GtkM Gtk.Widget)
initializeChessApp = do
  (appStateD, appStateT) <- initialAppState
  run <- myRunner
  pure $ runMarkupWithTwo windowRunner run (handleEvents appStateD appStateT) (root appStateD)


hotReload :: IO ()
hotReload = initializeChessApp >>= hotReloadMarkupWithoutAsking
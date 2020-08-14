module Main where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import AppState
import Handler
import UI.Root
import UI.MyRunner

import qualified GI.Gtk as Gtk
import Control.Monad.IO.Class (MonadIO(liftIO))
import HotReload (hotReloadMarkupWithoutAsking)

main :: IO ()
main = do
  (appStateD, appStateT) <- initialAppState
  run <- myRunner
  runGtkWidget $ runMarkupWithTwo windowRunner run (handleEvents appStateD appStateT) (root appStateD)

chessWidget :: GtkM Gtk.Widget
chessWidget = do
  (appStateD, appStateT) <- liftIO initialAppState
  run <- liftIO myRunner
  runMarkupWithTwo windowRunner run (handleEvents appStateD appStateT) (root appStateD)


hotReload :: IO ()
hotReload = hotReloadMarkupWithoutAsking chessWidget
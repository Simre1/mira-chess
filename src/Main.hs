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

main :: IO ()
main = initializeChessApp >>= runGtkWidget

initializeChessApp :: IO (GtkM Gtk.Widget)
initializeChessApp = do
  (appStateD, appStateT) <- initialAppState
  runner <- makeRunner
  let handle = 
        handleEvents (functorMap toBehavior appStateD) appStateT
  pure $ runMarkupWithTwo windowRunner runner handle (root appStateD)

hotReload :: IO ()
hotReload = initializeChessApp >>= hotReloadMarkupWithoutAsking
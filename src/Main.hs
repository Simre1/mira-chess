module Main where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import AppState
import Handler
import UI.Root
import UI.MyRunner


main :: IO ()
main = do
  (appStateD, appStateT) <- initialAppState
  run <- myRunner
  withCustomRunner (runMarkupWithTwo windowRunner run (handleEvents appStateD appStateT)) (root appStateD)
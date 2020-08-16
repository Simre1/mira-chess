module View.Runner where

import ReactiveMarkup
import ReactiveMarkup.Runners.Gtk
import View.Components.ChessBoard

import View.ViewM
import qualified GI.Gtk as Gtk

myRunner :: IO (Runner (GtkElements |-> '[ChessBoard]) (IO ()) (GtkM Gtk.Widget))
myRunner = do
  viewEnv <- initViewEnv
  pure $ widgetRunner |-> mapRunnerResult (runViewM viewEnv) liftGtkM runChessBoard

runChessBoard :: Runner '[ChessBoard] (IO ()) (ViewM Gtk.Widget)
runChessBoard = eventRun $ \(ChessBoard dynChessBoard) handleEvent -> do
  getPieceImage <- askEnv pieceImages
  liftGtkM $ runMarkup widgetRunner handleEvent (chessBoardMarkup getPieceImage dynChessBoard)
module UI.MyRunner where

import qualified GI.Gtk as Gtk
import qualified Data.Text as T


import UI.MyComponents.ChessBoard
import ReactiveMarkup
import ReactiveMarkup.Runners.Gtk

myRunner :: Runner (GtkElements |-> '[ChessBoard]) IO (GtkM Gtk.Widget)
myRunner = widgetRunner |-> runChessBoard

runChessBoard :: RunElement ChessBoard IO (GtkM Gtk.Widget)
runChessBoard (ChessBoard positionData) _ handleEvent =
  runMarkup widgetRunner handleEvent $ gridLayout (GridOptions 8 8) $ 
    makeSquare <$> [A1 .. H8]
  where 
    toGridPosition square = GridPosition c (7-r) 1 1
      where (r,c) = squareCoordinates square
    makeSquare square = 
      gridChild (toGridPosition square) $ button $ text $ T.pack $ show $
        piecePositions positionData square
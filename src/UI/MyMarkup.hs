module UI.MyMarkup where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import UI.MyComponents.ChessBoard

type MyMarkup = GtkElements |-> '[ChessBoard]



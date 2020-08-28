module View.Markup where

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup
import View.Components.ChessBoard

type MyMarkup = GtkElements |-> '[ChessBoard]

data AppState f = AppState (f String)
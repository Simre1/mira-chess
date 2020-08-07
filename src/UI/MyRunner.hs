module UI.MyRunner where

import AppState.Chess
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import ReactiveMarkup
import ReactiveMarkup.Runners.Gtk
import UI.MyComponents.ChessBoard

myRunner :: Runner (GtkElements |-> '[ChessBoard]) IO (GtkM Gtk.Widget)
myRunner = widgetRunner |-> runChessBoard

runChessBoard :: RunElement ChessBoard IO (GtkM Gtk.Widget)
runChessBoard (ChessBoard positionData) _ handleEvent = do
  selectedFieldRef <- liftIO (newIORef Nothing)
  (activeSquare, activateSquare) <- liftIO $ newDynamic Nothing
  let handleButtonClick square = do
        selectedField <- readIORef selectedFieldRef
        maybe
          (writeIORef selectedFieldRef (Just square) *> triggerEvent activateSquare (Just square))
          ( \previousSquare -> do
              handleEvent (Move previousSquare square)
              writeIORef selectedFieldRef Nothing
              triggerEvent activateSquare Nothing
          )
          selectedField
  runMarkup widgetRunner handleEvent $
    gridLayout (GridOptions 8 8) $
      makeSquare handleButtonClick activeSquare <$> [A1 .. H8]
  where
    toGridPosition square = GridPosition c (7 - r) 1 1
      where
        (r, c) = squareCoordinates square
    makeSquare handleButtonClick activeSquare square =
      gridChild (toGridPosition square) $
        handleEventIO
          (fmap (const Nothing) . handleButtonClick)
          $ dynamicMarkup activeSquare $ \active -> button $
            (if active == Just square
              then (%% fontColour green)
              else expandOptions)
            (text $ T.pack $ show $ piecePositions positionData square)
              %% (onClick square)
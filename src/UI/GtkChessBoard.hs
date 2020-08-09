module UI.GtkChessBoard where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Cairo
import qualified Graphics.Rendering.Cairo as C
import Foreign.Ptr (castPtr)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

import AppState.Chess

import ReactiveMarkup.SimpleEvents

import ReactiveMarkup.Runners.Gtk

import UI.MyComponents.ChessBoard
import Events
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (when, forM_)
import Debug.Trace (traceShowId)
import Data.IORef (writeIORef, readIORef, IORef, newIORef)

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = Gtk.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

customChessBoard :: Dynamic ChessPosition -> (Move -> IO ()) -> GtkM Gtk.Widget
customChessBoard chessPosition handleEvent = do
  drawingArea <- Gtk.new Gtk.DrawingArea []
  Gtk.widgetAddEvents drawingArea [Gdk.EventMaskAllEventsMask]
  unregisterWidgetUpdate <- liftIO $ reactimate (toEvent chessPosition) $ simpleEventHandler $ \_ -> Gtk.widgetQueueDraw drawingArea
  Gtk.on drawingArea #destroy (liftES unregisterWidgetUpdate)
  selectedFieldRef <- liftIO (newIORef Nothing)
  Gtk.onWidgetDraw drawingArea $
    \context -> do
      squareSize <- calcSquareSize drawingArea
      chessPosition <- current $ toBehavior chessPosition
      renderWithContext context (rendering selectedFieldRef squareSize chessPosition)
      pure True
  Gtk.onWidgetButtonPressEvent drawingArea $ \eventButton -> do
    mouseButton <- Gdk.getEventButtonButton eventButton
    when (mouseButton == 1) $ do
        eventX <- floor <$> Gdk.getEventButtonX eventButton
        eventY <- floor <$> Gdk.getEventButtonY eventButton
        squareSize <- calcSquareSize drawingArea
        window <- Gtk.widgetGetToplevel drawingArea
        (_, widgetX, widgetY) <- Gtk.widgetTranslateCoordinates window drawingArea eventX eventY
        let squareId = (floor $ fromIntegral widgetX / squareSize) + (floor $ 8 - (fromIntegral widgetY / squareSize)) * 8
        if squareId >= 0 && squareId < 64
          then clickField drawingArea selectedFieldRef $ toEnum squareId
          else pure ()
        pure ()
    pure False
  Gtk.toWidget drawingArea
  where 
    clickField :: Gtk.DrawingArea -> IORef (Maybe Square) -> Square -> IO ()
    clickField drawingArea selectedFieldRef square = do
      selectedField <- readIORef selectedFieldRef
      case selectedField of
        Just selected -> do
          writeIORef selectedFieldRef Nothing
          handleEvent (Move selected square)
        Nothing -> do
          writeIORef selectedFieldRef $ Just square
          Gtk.widgetQueueDraw drawingArea
      pure ()
    calcSquareSize :: MonadIO m => Gtk.DrawingArea -> m Double
    calcSquareSize drawingArea = do
      width <- Gtk.widgetGetAllocatedWidth drawingArea
      height <- Gtk.widgetGetAllocatedHeight drawingArea
      pure $ fromIntegral (min width height) / 8
    rendering :: IORef (Maybe Square) -> Double -> ChessPosition -> Render ()
    rendering selectedFieldRef squareSize chessPosition = do
      C.withTargetSurface $ \surface -> do
        drawBoard
        drawSelectedField
        drawPieces
        pure ()
      where
        drawSelectedField :: Render ()
        drawSelectedField = do
          selectedSquare <- liftIO $ readIORef selectedFieldRef
          liftIO $ print selectedSquare
          case selectedSquare of
            Nothing -> pure ()
            Just square -> do
                let (r,c) = squareCoordinates square
                    (x,y) = (squareSize * (fromIntegral c),squareSize * (fromIntegral $ 7-r))
                C.save
                C.rectangle x y (squareSize) (squareSize)
                C.setSourceRGBA 0 1 0 0.3
                C.fill
                C.restore
        drawPieces :: Render ()
        drawPieces = do
          C.save
          forM_ [A1 .. H8] $ \square ->
            case getPiece chessPosition square of
              Nothing -> pure ()
              Just (colour, piece) -> do
                let (r,c) = squareCoordinates square
                    (x,y) = (squareSize * (fromIntegral c),squareSize * (fromIntegral $ 7-r))
                C.newPath
                C.lineTo (x+squareSize/8) (y+squareSize/2)
                C.showText $ show piece
          C.restore
        drawBoard :: Render ()
        drawBoard = do
          C.save
          forM_ [A1 .. H8] $ \square -> do
            let (r,c) = squareCoordinates square
                (x,y) = (squareSize * (fromIntegral c),squareSize * (fromIntegral $ 7-r))
            C.newPath
            C.rectangle x y (squareSize) (squareSize)
            if even (r + c)
              then C.setSourceRGB 1 1 1
              else C.setSourceRGB 0.5 0.5 0.5
            C.fill
            C.setSourceRGB 0 0 0
          C.restore
      

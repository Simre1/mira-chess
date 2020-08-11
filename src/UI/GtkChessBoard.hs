module UI.GtkChessBoard where

import ReactiveMarkup.SimpleEvents
import AppState.Chess
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import ReactiveMarkup.Runners.Gtk
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.Rasterific
import Codec.Picture (imageData, convertRGBA8, readImage, PixelRGBA8(..))

import UI.RenderRasterific
import Control.Monad (forM_, when)
import Graphics.Rasterific.Texture (withSampler, transformTexture, sampledImageTexture, uniformTexture)
import Graphics.Rasterific.Transformations (translate, scale, applyTransformation)
import Debug.Trace (traceShowId)

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
      drawing <- rendering selectedFieldRef (fromIntegral squareSize) chessPosition
      renderRasterific (squareSize*8) (squareSize*8) context drawing
      pure True
  Gtk.onWidgetButtonPressEvent drawingArea $ \eventButton -> do
    mouseButton <- Gdk.getEventButtonButton eventButton
    when (mouseButton == 1) $ do
        eventX <- floor <$> Gdk.getEventButtonX eventButton
        eventY <- floor <$> Gdk.getEventButtonY eventButton
        squareSize <- calcSquareSize drawingArea
        window <- Gtk.widgetGetToplevel drawingArea
        (_, widgetX, widgetY) <- Gtk.widgetTranslateCoordinates window drawingArea eventX eventY
        let squareId = (floor $ fromIntegral widgetX / fromIntegral squareSize) + (floor $ 8 - (fromIntegral widgetY / fromIntegral squareSize)) * 8
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
      currentChessPosition <- current (toBehavior chessPosition)
      case selectedField of
        Just selected -> do
          if (isLegalMove currentChessPosition (Move selected square Nothing)) 
            then do
              writeIORef selectedFieldRef Nothing
              handleEvent (Move selected square Nothing)
            else
              when (fmap fst (getPiece currentChessPosition square) == Just (getActiveColour currentChessPosition)) $ do
                writeIORef selectedFieldRef (Just square)
                Gtk.widgetQueueDraw drawingArea
        Nothing -> do
          when (fmap fst (getPiece currentChessPosition square) == Just (getActiveColour currentChessPosition)) $ do
            writeIORef selectedFieldRef $ Just square
            Gtk.widgetQueueDraw drawingArea
      pure ()
    calcSquareSize :: MonadIO m => Gtk.DrawingArea -> m Int
    calcSquareSize drawingArea = do
      width <- Gtk.widgetGetAllocatedWidth drawingArea
      height <- Gtk.widgetGetAllocatedHeight drawingArea
      pure $ fromIntegral (min width height) `quot` 8
    rendering :: IORef (Maybe Square) -> Float -> ChessPosition -> IO (Drawing PixelRGBA8 ())
    rendering selectedFieldRef squareSize chessPosition = do
        d1 <- drawBoard 
        d2 <- drawSelectedField
        d3 <- drawPieces
        pure $ d1 >> d2 >> d3
      where
        drawBoard :: IO (Drawing PixelRGBA8 ())
        drawBoard = pure $
          forM_ [A1 .. H8] $ \square -> do
            let (r,c) = squareCoordinates square
            let colour = if even (r+c)
                  then PixelRGBA8 255 255 255 255
                  else PixelRGBA8 120 120 120 255
            withTexture (uniformTexture $ colour) $ do
              fill $ transform (applyTransformation $ scale squareSize squareSize) $ rectangle (fromIntegral <$> V2 c r) 1 1
        drawSelectedField :: IO (Drawing PixelRGBA8 ())
        drawSelectedField = do
          maybeSelected <- readIORef selectedFieldRef
          pure $ case maybeSelected of
            Nothing -> pure ()
            Just square -> do
              let (r,c) = squareCoordinates square
              withTexture (uniformTexture $ PixelRGBA8 0 255 0 120) $ do
                fill $ transform (applyTransformation $ scale squareSize squareSize) $ rectangle (fromIntegral <$> V2 c (7-r)) 1 1

        drawPieces :: IO (Drawing PixelRGBA8 ())
        drawPieces = do
          foldMap drawFigure [A1 .. H8]
          where 
            drawFigure :: Square -> IO (Drawing PixelRGBA8 ())
            drawFigure square = do
              let maybePiece = getPiece chessPosition square
              case maybePiece of
                Nothing -> pure $ pure ()
                Just (c,p) -> do
                  image <- readImage $ mapToFilePath (c,p)
                  case image of
                    Left err -> error err
                    Right dynamicImage -> do
                      let (r,c) = squareCoordinates square
                          texture = withSampler SamplerRepeat $ transformTexture (scale (110/squareSize) (110/squareSize)) $ sampledImageTexture $ convertRGBA8 dynamicImage
                      print (r,c)
                      pure $ withTexture texture $ do
                        fill $ rectangle ((squareSize*).fromIntegral <$> V2 c (7-r)) 110 110
                      -- pure $ withTexture () $ do
                      --    fill $ rectangle (((squareSize *) . fromIntegral) <$> V2 c (7 - r)) squareSize squareSize
              where
                mapToFilePath :: (PieceColour, Piece) -> String
                mapToFilePath x = case x of
                  (White, King) -> "assets/board/merida/white_king.png"
                  (White, Pawn) -> "assets/board/merida/white_pawn.png"
                  (White, Bishop) -> "assets/board/merida/white_bishop.png"
                  (White, Knight) -> "assets/board/merida/white_knight.png"
                  (White, Rook) -> "assets/board/merida/white_rook.png"
                  (White, Queen) -> "assets/board/merida/white_queen.png"
                  (Black, King) -> "assets/board/merida/black_king.png"
                  (Black, Pawn) -> "assets/board/merida/black_pawn.png"
                  (Black, Bishop) -> "assets/board/merida/black_bishop.png"
                  (Black, Knight) -> "assets/board/merida/black_knight.png"
                  (Black, Rook) -> "assets/board/merida/black_rook.png"
                  (Black, Queen) -> "assets/board/merida/black_queen.png"

 


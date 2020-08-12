module UI.GtkChessBoard where

import ReactiveMarkup.SimpleEvents
import AppState.Chess
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import ReactiveMarkup.Runners.Gtk
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)
import Codec.Picture (DynamicImage, imageData, convertRGBA8, readImage, PixelRGBA8(..))
import UI.RenderDiagram

import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Prelude hiding (Dynamic)
import Diagrams.TwoD.Image (embeddedImage, image)
import Control.Monad (when)


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
      renderDiagram (squareSize*8) (squareSize*8) context drawing
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
    rendering :: IORef (Maybe Square) -> Float -> ChessPosition -> IO (Diagram Cairo)
    rendering selectedFieldRef squareSize chessPosition = 
      liftA3 (\p f b -> atop p $ atop f b) drawPieces drawSelectedField drawBoard
      where
        drawPieces :: IO (Diagram Cairo)
        drawPieces = do
          board :: [[Diagram Cairo]] <- sequenceA $ fmap sequenceA $ fmap drawPiece <$>
                [ [A8 .. H8]
                , [A7 .. H7]
                , [A6 .. H6]
                , [A5 .. H5]
                , [A4 .. H4]
                , [A3 .. H3]
                , [A2 .. H2]
                , [A1 .. H1]
                ]
          pure $ foldr (===) mempty $ ((foldr (|||) mempty) <$> board)
          where 
            drawPiece :: Square -> IO (Diagram Cairo)
            drawPiece square = do
              let maybePiece = getPiece chessPosition square
              case maybePiece of
                Nothing -> pure $ rect 1 1 # lw 0
                Just (c,p) -> do
                  img :: Either String DynamicImage <- readImage $ mapToFilePath (c,p)
                  pure $ case img of
                    Left err -> error err
                    Right (dynamicImage) -> scale (1/110) $
                      image $ embeddedImage dynamicImage
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
        drawSelectedField :: IO (Diagram Cairo)
        drawSelectedField = do
          selectedSquare <- readIORef selectedFieldRef
          pure $ case selectedSquare of
            Nothing -> mempty
            Just square -> 
              let (r,c) = squareCoordinates square
              in rect 1 1 # lw 0 # fc green # translate (fromIntegral <$> r2 (c, -7+r))
        drawBoard :: IO (Diagram Cairo)
        drawBoard = pure $
          let board = 
                [ [A8 .. H8]
                , [A7 .. H7]
                , [A6 .. H6]
                , [A5 .. H5]
                , [A4 .. H4]
                , [A3 .. H3]
                , [A2 .. H2]
                , [A1 .. H1]
                ]
          in foldr (===) mempty $ (foldr (|||) mempty . fmap drawSquare) <$> board
          where 
            drawSquare square = 
              let (r,c) = squareCoordinates square
              in rect 1 1 # lw 0 # if (even $ r + c)
                then fc white
                else fc grey

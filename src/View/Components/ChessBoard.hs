module View.Components.ChessBoard where

import Codec.Picture (DynamicImage)
import Data.Chess
import Diagrams.Prelude hiding (Dynamic, noOps, text)
import Diagrams.TwoD.Image (embeddedImage)
import Events
import ReactiveMarkup hiding (atop)
import ReactiveMarkup.Runners.Gtk (Cairo)

data ChessBoard deriving (Typeable)

data instance Element ChessBoard elems e = e ~ ChessGameEvent => ChessBoard (Dynamic ChessGame)

chessBoard :: Dynamic ChessGame -> Markup '[ChessBoard] '[] (ChessGameEvent)
chessBoard = toMarkup . ChessBoard

chessBoardMarkup :: ((PieceColour, Piece) -> DynamicImage) -> Dynamic ChessGame -> Markup '[List '[]] '[DynamicStateIO, FlowLayout '[], DrawingBoard '[DrawDynamicDiagram Cairo, MouseClickWithPosition, AspectRatio], Button '[Text, Click]] ChessGameEvent
chessBoardMarkup getPieceImage chessPosition =
  list noOps $
    chessBoardWidget
      +: [ flowLayout noOps $
             button (text "Previous" // onClick PreviousMove)
               +: [button (text "Next" // onClick NextMove)]
         ]
  where
    chessBoardWidget = dynamicStateIO Nothing update $ \selectedField ->
      let dynamicDiagram = rendering <$> liftA2 (,) selectedField (fst . currentPosition <$> chessPosition)
       in drawingBoard (drawDynamicDiagram dynamicDiagram // mouseClickWithPosition id // aspectRatio 1)
    update :: Maybe Square -> (Double, Double) -> IO (Maybe (Maybe Square), Maybe ChessGameEvent)
    update selectedField (x, y) = do
      let squareId = (floor $ x * 8) + (floor $ 8 - 8 * y) * 8
      case squareId >= 0 && squareId < 64 of
        False -> pure (Nothing, Nothing)
        True -> do
          let square = toEnum squareId
          currentChessGame <- current (toBehavior chessPosition)
          let currentChessPosition = fst $ currentPosition currentChessGame
          pure $ case selectedField of
            Just selected -> do
              if (isLegalMove currentChessPosition (makeMove selected square Nothing))
                then (Just Nothing, Just $ MakeMove $ makeMove selected square Nothing)
                else
                  if (fmap fst (getPiece currentChessPosition square) == Just (getActiveColour currentChessPosition))
                    then (Just (Just square), Nothing)
                    else (Nothing, Nothing)
            Nothing -> do
              if (fmap fst (getPiece currentChessPosition square) == Just (getActiveColour currentChessPosition))
                then (Just (Just square), Nothing)
                else (Nothing, Nothing)
    rendering :: (Maybe Square, ChessPosition) -> (Diagram Cairo)
    rendering (selectedSquare, chessPosition) =
      drawPieces `atop` drawSelectedField `atop` drawBoard
      where
        drawPieces :: (Diagram Cairo)
        drawPieces =
          let board :: [[Diagram Cairo]] =
                fmap drawPiece
                  <$> [ [A8 .. H8],
                        [A7 .. H7],
                        [A6 .. H6],
                        [A5 .. H5],
                        [A4 .. H4],
                        [A3 .. H3],
                        [A2 .. H2],
                        [A1 .. H1]
                      ]
           in foldr (===) mempty $ ((foldr (|||) mempty) <$> board)
          where
            drawPiece :: Square -> (Diagram Cairo)
            drawPiece square =
              let maybePiece = getPiece chessPosition square
               in case maybePiece of
                    Nothing -> rect 1 1 # lw 0
                    Just (c, p) ->
                      scale (1 / 110) $ image $ embeddedImage $ getPieceImage (c, p)
        drawSelectedField :: (Diagram Cairo)
        drawSelectedField =
          case selectedSquare of
            Nothing -> mempty
            Just square ->
              let (r, c) = squareCoordinates square
               in rect 1 1 # lw 0 # fillColor (black `withOpacity` 0.3) # translate (fromIntegral <$> r2 (c, -7 + r))
        drawBoard :: (Diagram Cairo)
        drawBoard =
          let board =
                [ [A8 .. H8],
                  [A7 .. H7],
                  [A6 .. H6],
                  [A5 .. H5],
                  [A4 .. H4],
                  [A3 .. H3],
                  [A2 .. H2],
                  [A1 .. H1]
                ]
           in foldr (===) mempty $ (foldr (|||) mempty . fmap drawSquare) <$> board
          where
            drawSquare square =
              let (r, c) = squareCoordinates square
               in rect 1 1 # lw 0
                    # if (even $ r + c)
                      then fc white
                      else fc $ sRGB24 210 160 110

-- chessPositionToPositionData :: ChessPosition -> PositionData
-- chessPositionToPositionData chessBoard = PositionData
--   { piecePositions = getPiece chessBoard
--   , activeColour = getActiveColour chessBoard
--   }
module UI.MyRunner where

import AppState.Chess
import Codec.Picture (readImage)
import Codec.Picture.Types (DynamicImage)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Text as T
import Data.Functor.Identity
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Prelude hiding (Dynamic)
import Diagrams.TwoD.Image (embeddedImage)
import qualified GI.Gtk as Gtk
import ReactiveMarkup hiding (atop)
import ReactiveMarkup.Runners.Gtk
import System.IO.Unsafe (unsafePerformIO)
import UI.MyComponents.ChessBoard
import qualified Data.Map as M
import Data.Maybe (fromJust)

myRunner :: IO (Runner (GtkElements |-> '[ChessBoard]) (IO ()) (GtkM Gtk.Widget))
myRunner = do
  chessEnv <- initChessEnv
  pure $ widgetRunner |-> mapRunnerResult (fromChessM chessEnv) liftGtkM runChessBoard
  where
    fromChessM :: ChessEnv -> ChessM a -> GtkM a
    fromChessM chessEnv (ChessM m) = runReaderT m chessEnv

initChessEnv :: IO ChessEnv
initChessEnv = do
  pieceImages <- traverse readImage $ snd <$> pieceImageLocations
  let pieceImageMap = M.fromList $ zipWith (,) (fst <$> pieceImageLocations) (handleError <$> pieceImages)
  pure $ ChessEnv (fromJust . flip M.lookup pieceImageMap)
  where
    handleError :: Either String a -> a
    handleError (Left e) = error e
    handleError (Right a) = a
    pieceImageLocations :: [((PieceColour, Piece), String)]
    pieceImageLocations =
      [ ((White, King), "assets/board/merida/white_king.png"),
        ((White, Pawn), "assets/board/merida/white_pawn.png"),
        ((White, Bishop), "assets/board/merida/white_bishop.png"),
        ((White, Knight), "assets/board/merida/white_knight.png"),
        ((White, Rook), "assets/board/merida/white_rook.png"),
        ((White, Queen), "assets/board/merida/white_queen.png"),
        ((Black, King), "assets/board/merida/black_king.png"),
        ((Black, Pawn), "assets/board/merida/black_pawn.png"),
        ((Black, Bishop), "assets/board/merida/black_bishop.png"),
        ((Black, Knight), "assets/board/merida/black_knight.png"),
        ((Black, Rook), "assets/board/merida/black_rook.png"),
        ((Black, Queen), "assets/board/merida/black_queen.png")
      ]

askEnv :: (ChessEnv -> a) -> ChessM a
askEnv f = ChessM $ asks f

newtype ChessM a = ChessM (ReaderT ChessEnv GtkM a) deriving (Functor, Applicative, Monad, MonadIO)

liftGtkM :: GtkM a -> ChessM a
liftGtkM = ChessM . ReaderT . const

data ChessEnv = ChessEnv
  { pieceImages :: (PieceColour, Piece) -> DynamicImage
  }

runChessBoard :: Runner '[ChessBoard] (IO ()) (ChessM Gtk.Widget)
runChessBoard = eventRun $ \(ChessBoard dynChessBoard) handleEvent -> do
  getPieceImage <- askEnv pieceImages
  liftGtkM $ runMarkup widgetRunner handleEvent (newChessBoard getPieceImage dynChessBoard) --customChessBoard dynChessBoard handleEvent

newChessBoard :: ((PieceColour, Piece) -> DynamicImage) -> Dynamic ChessPosition -> SimpleMarkup GtkElements Move
newChessBoard getPieceImage chessPosition = expandMarkup $
  dynamicStateIO Nothing update $ \selectedField ->
    let dynamicDiagram = rendering <$> liftA2 (,) selectedField chessPosition
     in drawingBoard (drawDynamicDiagram dynamicDiagram %% mouseClickWithPosition id %% aspectRatio 1)
  where
    update :: Maybe Square -> (Double, Double) -> IO (Maybe (Maybe Square), Maybe Move)
    update selectedField (x, y) = do
      let squareId = (floor $ x * 8) + (floor $ 8 - 8 * y) * 8
      case squareId >= 0 && squareId < 64 of
        False -> pure (Nothing, Nothing)
        True -> do
          let square = toEnum squareId
          currentChessPosition <- current (toBehavior chessPosition)
          pure $ case selectedField of
            Just selected -> do
              if (isLegalMove currentChessPosition (Move selected square Nothing))
                then (Just Nothing, Just $ Move selected square Nothing)
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
-- "#f4a460"
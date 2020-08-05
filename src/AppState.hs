module AppState where

import ReactiveMarkup.SimpleEvents
import Data.Maybe

import UI.MyComponents.ChessBoard

import qualified Game.Chess as C

import qualified Data.Map as M

data ChessPosition = ChessPosition C.Position

newtype ChessPositionId = ChessPositionId Int deriving (Eq, Num, Ord, Show)

data AppState f = AppState 
  { chessPositions :: f (M.Map ChessPositionId ChessPosition)
  }

initialAppState :: IO (AppState Dynamic, AppState EventTrigger)
initialAppState = do
  (chessPositionsD, chessPositionsT) <- newDynamic $ M.insert 0 (ChessPosition C.startpos) M.empty
  pure (AppState chessPositionsD, AppState chessPositionsT)

getChessPosition :: ChessPositionId -> AppState Dynamic -> Dynamic ChessPosition
getChessPosition id' state = fromJust . M.lookup id' <$> chessPositions state

chessPositionToPositionData :: ChessPosition -> PositionData
chessPositionToPositionData (ChessPosition position) =
  PositionData {
    piecePositions = \square -> mapColourPiece <$> C.pieceAt position (mapSquare square),
    activeColour = undefined
  }
  where
    mapColour :: C.Color -> Colour
    mapColour C.White = White
    mapColour C.Black = Black
    mapPiece :: C.PieceType -> Piece
    mapPiece p = case p of
      C.King -> King
      C.Queen -> Queen
      C.Bishop -> Bishop
      C.Knight -> Knight
      C.Rook -> Rook
      C.Pawn -> Pawn
    mapColourPiece (c,p) = (mapPiece p, mapColour c)
    mapSquare :: Square -> C.Sq
    mapSquare = toEnum . fromEnum

module AppState.Chess where

import qualified Game.Chess as C
import Control.Exception (try, catch)
import Data.Maybe (fromMaybe)

data PieceColour = White | Black deriving (Eq, Show)

type HalfMoves = Int

data Piece
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn 
  deriving (Eq, Show)

data Square
  = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
  | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
  | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
  | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
  | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
  | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
  | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
  | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 
  deriving (Enum, Eq, Show)

type Row = Int
type Column = Int

data Move = Move Square Square deriving (Show, Eq)

squareCoordinates :: Square -> (Row, Column)
squareCoordinates square = (i `div` 8, i `rem` 8)
  where i = fromEnum square

data ChessPosition = ChessPosition C.Position

getPiece :: ChessPosition -> Square -> Maybe (PieceColour, Piece)
getPiece (ChessPosition position) square = 
  mapColourPiece <$> C.pieceAt position (mapSquare square)
  where
    mapColour :: C.Color -> PieceColour
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
    mapColourPiece (c,p) = (mapColour c, mapPiece p)
    mapSquare :: Square -> C.Sq
    mapSquare = toEnum . fromEnum

getHalfMoves :: ChessPosition -> HalfMoves
getHalfMoves (ChessPosition position) = C.halfMoveClock position

getActiveColour :: ChessPosition -> PieceColour
getActiveColour (ChessPosition position) = mapColour $ C.color position
  where
    mapColour :: C.Color -> PieceColour
    mapColour C.White = White
    mapColour C.Black = Black

executeMove :: Move -> ChessPosition -> ChessPosition
executeMove (Move f t) (ChessPosition position) = ChessPosition $ fromMaybe position $ C.doPly position (C.move (mapSquare f) (mapSquare t))
  where
    mapSquare :: Square -> C.Sq
    mapSquare = toEnum . fromEnum

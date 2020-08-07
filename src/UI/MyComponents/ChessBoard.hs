module UI.MyComponents.ChessBoard where

import ReactiveMarkup

import AppState.Chess

data PositionData = PositionData 
  { piecePositions :: Square -> Maybe (PieceColour, Piece)
  , activeColour :: PieceColour
  }

data ChessBoard deriving Typeable

data instance Element ChessBoard elems e = e ~ Move => ChessBoard PositionData

chessBoard :: PositionData -> Markup '[ChessBoard] '[] (Move)
chessBoard = toMarkup . ChessBoard

chessPositionToPositionData :: ChessPosition -> PositionData
chessPositionToPositionData chessBoard = PositionData
  { piecePositions = getPiece chessBoard
  , activeColour = getActiveColour chessBoard
  }
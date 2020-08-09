module UI.MyComponents.ChessBoard where

import ReactiveMarkup

import AppState.Chess

data ChessBoard deriving Typeable

data instance Element ChessBoard elems e = e ~ Move => ChessBoard (Dynamic ChessPosition)

chessBoard :: Dynamic ChessPosition -> Markup '[ChessBoard] '[] (Move)
chessBoard = toMarkup . ChessBoard

-- chessPositionToPositionData :: ChessPosition -> PositionData
-- chessPositionToPositionData chessBoard = PositionData
--   { piecePositions = getPiece chessBoard
--   , activeColour = getActiveColour chessBoard
--   }
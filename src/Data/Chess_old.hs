module Data.Chess_old where

import qualified Game.Chess as C
import qualified Game.Chess.PGN as C
import Control.Exception (try, catch)
import Data.Maybe (fromMaybe)
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Tree as T
import Data.Foldable (Foldable(foldl'))
import Optics.Core (FoldableWithIndex(ifoldl'))
import Codec.Serialise

data PieceColour = White | Black deriving (Eq, Show, Ord, Generic)

instance Serialise PieceColour where

instance Hashable PieceColour where

type HalfMoves = Int

data Piece
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn 
  deriving (Eq, Show, Ord, Generic)

instance Hashable Piece where

instance Serialise Piece

data Square
  = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
  | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
  | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
  | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
  | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
  | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
  | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
  | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 
  deriving (Enum, Eq, Show, Generic)

instance Serialise Square

type Row = Int
type Column = Int

data Move = Move Square Square (Maybe Piece) deriving (Show, Eq, Generic)

instance Serialise Move where

squareCoordinates :: Square -> (Row, Column)
squareCoordinates square = (i `div` 8, i `rem` 8)
  where i = fromEnum square

data ChessPosition = ChessPosition C.Position deriving (Eq, Generic)

getPiece :: ChessPosition -> Square -> Maybe (PieceColour, Piece)
getPiece (ChessPosition position) square = 
  mapColourPiece <$> C.pieceAt position (fromSquare square)
  where mapColourPiece (c,p) = (toPieceColour c, toPiece p)


getHalfMoves :: ChessPosition -> HalfMoves
getHalfMoves (ChessPosition position) = C.halfMoveClock position

getActiveColour :: ChessPosition -> PieceColour
getActiveColour (ChessPosition position) = toPieceColour $ C.color position

executeMove :: Move -> ChessPosition -> ChessPosition
executeMove move (ChessPosition position) = ChessPosition $ fromMaybe position $ C.doPly position (toPly move)

isLegalMove :: ChessPosition -> Move -> Bool
isLegalMove (ChessPosition position) move = toPly move `elem` C.legalPlies position

startPosition :: ChessPosition
startPosition = ChessPosition C.startpos

toPly :: Move -> C.Ply
toPly (Move from to promo) = 
  let ply = C.move (fromSquare from) (fromSquare to)
  in maybe ply (C.promoteTo ply . fromPiece) promo

data MoveIndex = MainMove Int | Variation Int Int (MoveIndex) deriving (Generic, Eq, Show)

instance Serialise MoveIndex where

instance Hashable MoveIndex where

data Game = Game {
  moves :: HM.HashMap MoveIndex Move,
  currentMove :: MoveIndex,
  currentPositions :: ChessPosition,
  outcome :: Outcome
} deriving (Generic)

-- nextMove :: Game -> Game
-- nextMove game@(Game {moves, currentMove, currentPositions}) = nextMainMoveIndex

data Outcome = Win PieceColour | Draw | Undecided deriving (Show, Generic)

instance Serialise Outcome where

readPGN :: FilePath -> IO (Either String [Game])
readPGN filepath = fmap (\(C.PGN games) -> toGame <$> games) <$> C.readPGNFile filepath

-- Mappings

-- maybe works?
toGame :: C.Game -> Game
toGame (pgnFlags, (cOutcome, plyData)) =
  let outcome = toOutcome cOutcome
      moves = walkForest plyData HM.empty
  in Game moves (MainMove 0) startPosition outcome
  where 
    walkForest :: T.Forest C.PlyData -> HM.HashMap MoveIndex Move -> HM.HashMap MoveIndex Move
    walkForest [] hm = hm
    walkForest (x:xs) hm = walkMoveTree (MainMove 0) x $ ifoldl' (\i hm m -> walkMoveTree (newVariation i (MainMove 0)) m hm) hm xs
    walkMoveTree :: MoveIndex -> T.Tree C.PlyData -> HM.HashMap MoveIndex Move -> HM.HashMap MoveIndex Move
    walkMoveTree mI (T.Node (C.PlyData _ ply _) subNodes) hm =
      let (from, to, pieceType) = C.unpack ply
          addRootMove = HM.insert mI (Move (toEnum from) (toEnum to) (toPiece <$> pieceType))
      in addRootMove $ case subNodes of
        [] -> hm
        (m:ms) -> walkMoveTree (nextMainMoveIndex mI) m $ 
          ifoldl' (\i hm m -> walkMoveTree (newVariation i mI) m hm) hm ms

nextMainMoveIndex :: MoveIndex -> MoveIndex
nextMainMoveIndex (MainMove i) = MainMove (succ i)
nextMainMoveIndex (Variation i n mi) = Variation i n $ nextMainMoveIndex mi
newVariation :: Int -> MoveIndex -> MoveIndex
newVariation x (MainMove i) = Variation i x (MainMove 0)
newVariation x (Variation i n m) = Variation i n $ newVariation x m

toOutcome :: C.Outcome -> Outcome
toOutcome o = case o of
  C.Win c -> Win $ toPieceColour c
  C.Draw -> Draw
  C.Undecided -> Undecided
  
fromSquare :: Square -> C.Sq
fromSquare = toEnum . fromEnum

fromPiece :: Piece -> C.PieceType
fromPiece p = case p of
  King -> C.King
  Queen -> C.Queen
  Bishop -> C.Bishop
  Knight -> C.Knight
  Rook -> C.Rook
  Pawn -> C.Pawn

toPieceColour :: C.Color -> PieceColour
toPieceColour C.White = White
toPieceColour C.Black = Black

toPiece :: C.PieceType -> Piece
toPiece p = case p of
  C.King -> King
  C.Queen -> Queen
  C.Bishop -> Bishop
  C.Knight -> Knight
  C.Rook -> Rook
  C.Pawn -> Pawn
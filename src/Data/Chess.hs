module Data.Chess where

import Data.Foldable (Foldable (foldl'))
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty (..))
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import qualified Data.Tree as T
import GHC.Generics (Generic)
import qualified Game.Chess as C
import qualified Game.Chess.PGN as C
import Data.Hashable (hashUsing, Hashable(..))
import qualified Data.Text as T

newtype PieceColour = PieceColour {unwrapPieceColour :: C.Color} deriving (Eq, Show, Generic)

instance Hashable PieceColour where
instance Hashable C.Color where
  hashWithSalt i C.White = i * 11
  hashWithSalt i C.Black = i * 7

pattern White, Black :: PieceColour
pattern White = PieceColour C.White
pattern Black = PieceColour C.Black

type HalfMoves = Int

newtype Piece = Piece {unwrapPiece :: C.PieceType} deriving (Eq, Show, Generic)

instance Hashable Piece where

instance Hashable C.PieceType where
  hashWithSalt = hashUsing toInt
    where
      toInt :: C.PieceType -> Int
      toInt C.King = 1
      toInt C.Queen = 2
      toInt C.Bishop = 3
      toInt C.Knight = 4
      toInt C.Rook = 5
      toInt C.Pawn = 6

pattern King, Queen, Bishop, Knight, Rook, Pawn :: Piece
pattern King = Piece C.King
pattern Queen = Piece C.Queen
pattern Bishop = Piece C.Bishop
pattern Knight = Piece C.Knight
pattern Rook = Piece C.Rook
pattern Pawn = Piece C.Pawn

newtype Square = Square {unwrapSquare :: C.Sq} deriving (Eq, Show, Generic, Enum)

pattern A1, A2, A3, A4, A5, A6, A7, A8 :: Square
pattern A1 = Square C.A1
pattern A2 = Square C.A2
pattern A3 = Square C.A3
pattern A4 = Square C.A4
pattern A5 = Square C.A5
pattern A6 = Square C.A6
pattern A7 = Square C.A7

pattern A8 = Square C.A8
pattern B1, B2, B3, B4, B5, B6, B7, B8 :: Square
pattern B1 = Square C.B1
pattern B2 = Square C.B2
pattern B3 = Square C.B3
pattern B4 = Square C.B4
pattern B5 = Square C.B5
pattern B6 = Square C.B6
pattern B7 = Square C.B7
pattern B8 = Square C.B8

pattern C1, C2, C3, C4, C5, C6, C7, C8 :: Square
pattern C1 = Square C.C1
pattern C2 = Square C.C2
pattern C3 = Square C.C3
pattern C4 = Square C.C4
pattern C5 = Square C.C5
pattern C6 = Square C.C6
pattern C7 = Square C.C7
pattern C8 = Square C.C8
pattern D1, D2, D3, D4, D5, D6, D7, D8 :: Square

pattern D1 = Square C.D1
pattern D2 = Square C.D2
pattern D3 = Square C.D3
pattern D4 = Square C.D4
pattern D5 = Square C.D5
pattern D6 = Square C.D6
pattern D7 = Square C.D7
pattern D8 = Square C.D8
pattern E1, E2, E3, E4, E5, E6, E7, E8 :: Square
pattern E1 = Square C.E1

pattern E2 = Square C.E2
pattern E3 = Square C.E3
pattern E4 = Square C.E4
pattern E5 = Square C.E5
pattern E6 = Square C.E6
pattern E7 = Square C.E7
pattern E8 = Square C.E8
pattern F1, F2, F3, F4, F5, F6, F7, F8 :: Square
pattern F1 = Square C.F1
pattern F2 = Square C.F2

pattern F3 = Square C.F3
pattern F4 = Square C.F4
pattern F5 = Square C.F5
pattern F6 = Square C.F6
pattern F7 = Square C.F7
pattern F8 = Square C.F8
pattern G1, G2, G3, G4, G5, G6, G7, G8 :: Square
pattern G1 = Square C.G1
pattern G2 = Square C.G2
pattern G3 = Square C.G3

pattern G4 = Square C.G4
pattern G5 = Square C.G5
pattern G6 = Square C.G6
pattern G7 = Square C.G7
pattern G8 = Square C.G8
pattern H1, H2, H3, H4, H5, H6, H7, H8 :: Square
pattern H1 = Square C.H1
pattern H2 = Square C.H2
pattern H3 = Square C.H3
pattern H4 = Square C.H4

pattern H5 = Square C.H5
pattern H6 = Square C.H6
pattern H7 = Square C.H7
pattern H8 = Square C.H8

newtype Move = Move {unwrapMove :: C.Ply} deriving (Eq, Show, Generic)

type Row = Int
type Column = Int

newtype ChessPosition = ChessPosition {unwrapChessPosition :: C.Position} deriving (Eq, Generic)

newtype Outcome = Outcome {unwrapOutcome :: C.Outcome} deriving (Eq, Show, Generic)


getPiece :: ChessPosition -> Square -> Maybe (PieceColour, Piece)
getPiece (ChessPosition position) (Square sq) = do
  (c, p) <- C.pieceAt position sq
  pure (PieceColour c, Piece p)

getHalfMoves :: ChessPosition -> HalfMoves
getHalfMoves (ChessPosition position) = C.halfMoveClock position

getActiveColour :: ChessPosition -> PieceColour
getActiveColour (ChessPosition position) = PieceColour $ C.color position

executeMove :: Move -> ChessPosition -> Maybe ChessPosition
executeMove (Move ply) (ChessPosition position) = ChessPosition <$> C.doPly position ply

isLegalMove :: ChessPosition -> Move -> Bool
isLegalMove (ChessPosition position) (Move ply) = ply `elem` C.legalPlies position

startPosition :: ChessPosition
startPosition = ChessPosition C.startpos

makeMove :: Square -> Square -> Maybe Piece -> Move
makeMove (Square from) (Square to) promotion =
  let ply = C.move from to
   in Move $ maybe ply (C.promoteTo ply . unwrapPiece) promotion

squareCoordinates :: Square -> (Row, Column)
squareCoordinates (Square sq) = (i `div` 8, i `rem` 8)
  where
    i = fromEnum sq

blackPieceColour :: PieceColour
blackPieceColour = PieceColour C.Black

whitePieceColour :: PieceColour
whitePieceColour = PieceColour C.White

isWhite :: PieceColour -> Bool
isWhite (PieceColour C.White) = True
isWhite _ = False

isBlack :: PieceColour -> Bool
isBlack = not . isWhite

data ChessGame = ChessGame
  { moves :: HM.HashMap MoveIndex (ChessPosition, T.Text),
    currentMove :: MoveIndex,
    outcome :: Outcome
  }
  deriving (Generic)

data PositionDifference = PositionDifference
  { applyMove :: ChessPosition -> ChessPosition,
    undoMove :: ChessPosition -> ChessPosition,
    moveName :: T.Text
  }
  deriving (Generic)

positionAtMove :: MoveIndex -> ChessGame -> Maybe (ChessPosition, T.Text)
positionAtMove mI game =
  HM.lookup mI $ moves game

currentPosition :: ChessGame -> (ChessPosition, T.Text)
currentPosition game = 
  fromJust $ positionAtMove (currentMove game) game

focusMove :: MoveIndex -> ChessGame -> ChessGame
focusMove index game
  | HM.member index $ moves game = game {currentMove = index}
  | otherwise = game

insertMainMoveAtCurrent :: Move -> ChessGame -> ChessGame 
insertMainMoveAtCurrent m g = fromMaybe g $ do
  let (p,_) = currentPosition g
  c <- executeMove m p
  let h = HM.insert (nextIndex $ currentMove g) (c, T.pack . show . unwrapMove $ m) $ moves g
  pure $ g {moves = h}

focusNextMove :: ChessGame -> ChessGame
focusNextMove g = focusMove (nextIndex (currentMove g)) g

focusPreviousMove :: ChessGame -> ChessGame
focusPreviousMove g = focusMove (previousIndex (currentMove g)) g

foldGame :: (MoveIndex -> b -> b) -> b -> ChessGame -> b
foldGame f initial game = foldl' (\b index -> f index b) initial $ sort $ HM.keys $ moves game

newGame :: ChessGame
newGame = ChessGame (HM.insert (MainMove (-1)) (startPosition, "start") $ HM.empty) (MainMove (-1)) (Outcome C.Undecided)

data MoveIndex = Variation Int HalfMoves MoveIndex | MainMove HalfMoves deriving (Eq, Show, Generic)

instance Ord MoveIndex where
  (MainMove i1) `compare` (MainMove i2) = i1 `compare` i2
  (Variation x1 i1 index1) `compare` (Variation x2 i2 index2) = 
    let c1 = i1 `compare` i2
        c2 = x1 `compare` x2
        c3 = index1 `compare` index2
    in case c1 of
        EQ -> case c2 of
          EQ -> c3
          _ -> c2
        _ -> c1

instance Hashable MoveIndex

readPGN :: FilePath -> IO (Either String [ChessGame])
readPGN filepath = fmap (\(C.PGN games) -> toGame <$> games) <$> C.readPGNFile filepath

toGame :: C.Game -> ChessGame
toGame (pgnFlags, (cOutcome, plyData)) =
  let outcome = Outcome cOutcome
      moves = forestToHashMap (MainMove (-1)) (mapForest plyData) HM.empty
   in ChessGame moves (MainMove 0) outcome
  where
    forestToHashMap :: MoveIndex -> T.Forest a -> HM.HashMap MoveIndex a -> HM.HashMap MoveIndex a
    forestToHashMap index forest hm =
      case forest of
        [] -> hm
        [x] -> treeToHashMap (nextIndex index) x hm
        (x : xs) ->
          treeToHashMap (nextIndex index) x $
            snd $
              foldl' (\(i, hm') t -> (succ i, treeToHashMap (newVariation i index) t hm')) (0, hm) xs
      where
        treeToHashMap :: MoveIndex -> T.Tree a -> HM.HashMap MoveIndex a -> HM.HashMap MoveIndex a
        treeToHashMap index (T.Node root children) hm =
          forestToHashMap index children $ HM.insert index root hm
    mapForest :: T.Forest C.PlyData -> T.Forest (ChessPosition, T.Text)
    mapForest = fmap $ mapTree startPosition
      where
        mapTree :: ChessPosition -> T.Tree C.PlyData -> T.Tree (ChessPosition, T.Text)
        mapTree (ChessPosition parentPosition) (T.Node (C.PlyData _ ply _) children) =
          let currentPosition = C.unsafeDoPly parentPosition ply
           in T.Node (ChessPosition currentPosition, T.pack . show $ ply) (mapTree (ChessPosition currentPosition) <$> children)

nextIndex :: MoveIndex -> MoveIndex
nextIndex (MainMove i) = MainMove $ succ i
nextIndex (Variation i n mi) = Variation i n $ nextIndex mi

previousIndex :: MoveIndex -> MoveIndex
previousIndex (MainMove i) = MainMove $ min 0 $ pred i
previousIndex (Variation i n (MainMove 0)) = MainMove n
previousIndex (Variation i n mi) = Variation i n $ previousIndex mi

newVariation :: Int -> MoveIndex -> MoveIndex
newVariation x (MainMove i) = Variation x i $ MainMove 0
newVariation x (Variation i n m) = Variation i n $ newVariation x m

module View.ViewM where

import Control.Monad.IO.Class (MonadIO)
import ReactiveMarkup.Runners.Gtk (GtkM)
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import qualified Data.HashMap.Strict as HM

import Data.Chess
import Codec.Picture (readImage, DynamicImage)
import Data.Maybe (fromJust)

newtype ViewM a = ViewM (ReaderT ViewEnv GtkM a) deriving (Functor, Applicative, Monad, MonadIO)

data ViewEnv = ViewEnv
  { pieceImages :: (PieceColour, Piece) -> DynamicImage
  }

liftGtkM :: GtkM a -> ViewM a
liftGtkM = ViewM . ReaderT . const

runViewM :: ViewEnv -> ViewM a -> GtkM a
runViewM env (ViewM m) = runReaderT m env

askEnv :: (ViewEnv -> a) -> ViewM a
askEnv f = ViewM $ asks f

initViewEnv :: IO ViewEnv
initViewEnv = do
  pieceImages <- traverse readImage $ snd <$> pieceImageLocations
  let pieceImageMap = HM.fromList $ zipWith (,) (fst <$> pieceImageLocations) (handleError <$> pieceImages)
  pure $ ViewEnv (fromJust . flip HM.lookup pieceImageMap)
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
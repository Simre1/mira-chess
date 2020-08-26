module Data.Database where

import Database.Selda.SQLite
import Database.Selda.Backend
import Database.Selda
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as S

import Data.Chess
class DBData a where
  data DB a
  dbTo :: a -> DB a
  dbFrom :: DB a -> a

data Database = Database (SeldaConnection SQLite)

newtype DatabaseM a = DatabaseM (SeldaT SQLite IO a) deriving (Functor, Applicative, Monad)

createDatabase :: FilePath -> IO Database
createDatabase  filePath = do
  database <- Database <$> sqliteOpen filePath
  runDatabaseM database initializeTables
  pure database

closeDatabase :: Database -> IO ()
closeDatabase (Database conn) = seldaClose conn

instance DBData ChessGame where
  data DB ChessGame = DBChessGame 
    { cgId :: ID (DB ChessGame)
    , cgOutcome :: Int
    , cgMoves :: BS.ByteString
    } deriving (Generic, Show)
  dbTo (game) = 
    DBChessGame def (fromEnum $ outcome game) (BSL.toStrict $ S.serialise $ moves game)
    where 
  dbFrom (DBChessGame _ o m) = ChessGame 
    { moves = S.deserialise $ BSL.fromStrict m
    , outcome = toEnum o
    , currentMove = MainMove 0
    }

instance SqlRow (DB ChessGame)

chessGamesTable :: Table (DB ChessGame)
chessGamesTable = table "chessGamesTable" [#cgId :- autoPrimary]

initializeTables :: DatabaseM ()
initializeTables = DatabaseM $ do
  tryCreateTable chessGamesTable

insertChessGames :: [DB ChessGame] -> DatabaseM ()
insertChessGames = DatabaseM . insert_ chessGamesTable

getChessGame :: DatabaseM [DB ChessGame]
getChessGame = DatabaseM $ query $ select chessGamesTable

runDatabaseM :: Database -> DatabaseM a -> IO a
runDatabaseM (Database conn) (DatabaseM selda) = runSeldaT selda conn

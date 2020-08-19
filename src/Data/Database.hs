module Data.Database where

import Database.Selda.SQLite
import Database.Selda.Backend
import Database.Selda
import qualified Data.ByteString as BS

data Database = Database (SeldaConnection SQLite)

newtype DatabaseM a = DatabaseM (SeldaT SQLite IO a) deriving (Functor, Applicative, Monad)

createDatabase :: FilePath -> IO Database
createDatabase  filePath = do
  database <- Database <$> sqliteOpen filePath
  runDatabaseM database initializeTables
  pure database

closeDatabase :: Database -> IO ()
closeDatabase (Database conn) = seldaClose conn

data DBChessGame = DBChessGame
  { cgId :: ID DBChessGame
  , cgData :: BS.ByteString
  } deriving (Generic, Show)

chessGames :: Table DBChessGame
chessGames = table "chessGames" [#cgId :- autoPrimary]

instance SqlRow DBChessGame

initializeTables :: DatabaseM ()
initializeTables = DatabaseM $ do
  tryCreateTable chessGames

insertChessGames :: [DBChessGame] -> DatabaseM ()
insertChessGames = DatabaseM . insert_ chessGames

getChessGame :: DatabaseM [DBChessGame]
getChessGame = DatabaseM $ query $ select chessGames

runDatabaseM :: Database -> DatabaseM a -> IO a
runDatabaseM (Database conn) (DatabaseM selda) = runSeldaT selda conn
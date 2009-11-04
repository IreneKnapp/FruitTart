module Network.FruitTart.Util.Database where

import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Numeric
import System.Locale

import qualified Data.SQLite3 as SQL
import Data.SQLite3 (SQLData(..))
import Network.FruitTart.Util.Types


earlyRun :: SQL.Database -> String -> IO ()
earlyRun database query = do
  statement <- SQL.prepare database query
  SQL.step statement
  SQL.finalize statement


earlyRun' :: SQL.Database -> String -> [SQL.SQLData] -> IO ()
earlyRun' database query bindings = do
  statement <- SQL.prepare database query
  SQL.bind statement bindings
  SQL.step statement
  SQL.finalize statement


earlyEval :: SQL.Database -> String -> IO SQL.SQLData
earlyEval database query = do
  statement <- SQL.prepare database query
  SQL.step statement
  result <- SQL.column statement 0
  SQL.finalize statement
  return result


query :: String -> [SQL.SQLData] -> FruitTart [[SQL.SQLData]]
query text bindings = do
  FruitTartState { database = database } <- get
  statement <- liftIO $ SQL.prepare database text
  liftIO $ SQL.bind statement bindings
  result <- query' statement
  liftIO $ SQL.finalize statement
  return result
  where query' statement = do
          stepResult <- liftIO $ SQL.step statement
          case stepResult of
            SQL.Row -> do
                   row <- liftIO $ SQL.columns statement
                   remainder <- query' statement
                   return $ [row] ++ remainder
            SQL.Done -> return []


getTimestamp :: MonadIO m => m Int64
getTimestamp = liftIO getPOSIXTime >>= return . floor


timestampToString :: Int64 -> String
timestampToString timestamp
    = formatTime defaultTimeLocale
                 "%Y-%m-%d %I:%M UTC"
                 (posixSecondsToUTCTime $ realToFrac timestamp)


byteSizeToString :: Int64 -> String
byteSizeToString byteSize
    = if fromIntegral byteSize < 2**10
      then (show byteSize) ++ " bytes"
      else if fromIntegral byteSize < 2**20
           then (showFFloat (Just 2) (fromIntegral byteSize / 2**10) "") ++ " KiB"
           else if fromIntegral byteSize < 2**30
                then (showFFloat (Just 2) (fromIntegral byteSize / 2**20) "") ++ "MiB"
                else (showFFloat (Just 2) (fromIntegral byteSize / 2**30) "") ++ "GiB"

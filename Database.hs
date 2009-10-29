module Database where

import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Numeric
import System.Locale

import qualified SQLite3 as SQL
import SQLite3 (SQLData(..))
import Types


query1 :: String -> [SQL.SQLData] -> Buglist [SQL.SQLData]
query1 text bindings = do
  BuglistState { database = database } <- get
  statement <- liftIO $ SQL.prepare database text
  liftIO $ SQL.bind statement bindings
  liftIO $ SQL.step statement
  result <- liftIO $ SQL.columns statement
  liftIO $ SQL.finalize statement
  return result


query :: String -> [SQL.SQLData] -> Buglist [[SQL.SQLData]]
query text bindings = do
  BuglistState { database = database } <- get
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

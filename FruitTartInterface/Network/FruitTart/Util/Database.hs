module Network.FruitTart.Util.Database where

import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Numeric
import System.Locale

import Database.SQLite3
import Network.FruitTart.Util.Types


earlyQuery :: Database -> String -> [SQLData] -> IO [[SQLData]]
earlyQuery database query bindings = do
  statement <- prepare database query
  bind statement bindings
  result <- query' statement
  finalize statement
  return result
  where query' statement = do
          stepResult <- step statement
          case stepResult of
            Row -> do
              row <- columns statement
              remainder <- query' statement
              return $ [row] ++ remainder
            Done -> return []


query :: String -> [SQLData] -> FruitTart [[SQLData]]
query text bindings = do
  FruitTartState { database = database } <- get
  statement <- liftIO $ prepare database text
  liftIO $ bind statement bindings
  result <- query' statement
  liftIO $ finalize statement
  return result
  where query' statement = do
          stepResult <- liftIO $ step statement
          case stepResult of
            Row -> do
                   row <- liftIO $ columns statement
                   remainder <- query' statement
                   return $ [row] ++ remainder
            Done -> return []


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

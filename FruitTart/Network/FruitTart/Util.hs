module Network.FruitTart.Util (getTimestamp,
                               timestampToString,
                               byteSizeToString)
  where


import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Numeric
import System.Locale

import Database.SQLite3
import Network.FruitTart.Types


getTimestamp :: MonadIO m => m Int64
getTimestamp = liftIO getPOSIXTime >>= return . floor


timestampToString :: Int64 -> String
timestampToString timestamp
    = formatTime defaultTimeLocale
                 "%Y-%m-%d %H:%M UTC"
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

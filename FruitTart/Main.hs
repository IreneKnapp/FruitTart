module Main (main) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.FastCGI
import System.Environment
import System.Exit
import System.IO.Unsafe

import Database.SQLite3
import Network.FruitTart.Types
import Network.FruitTart.Util
import Network.FruitTart.Dispatcher


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  captchaCacheMVar <- newMVar $ Map.empty
  state <- return $ FruitTartState {
             database = database,
             captchaCacheMVar = captchaCacheMVar,
             sessionID = Nothing,
             maybeCurrentPage = Nothing,
             maybeControllerName = Nothing
           }
  acceptLoop forkIO $ evalStateT processRequest state
  return ()

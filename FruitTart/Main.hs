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
import Network.FruitTart.Database
import Network.FruitTart.Design
import Network.FruitTart.Dispatcher
import Network.FruitTart.Types


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  design <- loadDesign database
  designMVar <- newMVar design
  captchaCacheMVar <- newMVar $ Map.empty
  state <- return $ FruitTartState {
             database = database,
             designMVar = designMVar,
             captchaCacheMVar = captchaCacheMVar,
             sessionID = Nothing,
             maybeCurrentPage = Nothing,
             maybeControllerName = Nothing
           }
  acceptLoop forkIO $ evalStateT processRequest state
  return ()

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
import System.IO

import Database.SQLite3
import Network.FruitTart.Types
import Network.FruitTart.Util.Database


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  functionsFile <- openFile "functions.txt" WriteMode
  templatesFile <- openFile "templates.txt" WriteMode
  functions
    <- earlyQuery database
                  "SELECT id, module, name FROM functions ORDER BY module, name"
                  []
  mapM_ (\[SQLInteger id, SQLText moduleName, SQLText properName] -> do
           let headerLine = moduleName ++ "." ++ properName ++ ": " ++ show id
               headerStandoffLine
                 = replicate (length headerLine) '='
           putStrLn headerStandoffLine
           putStrLn headerLine
           putStrLn headerStandoffLine
           putStrLn ""
           parameters
             <- earlyQuery database
                           ("SELECT name FROM function_parameters "
                            ++ "WHERE function = ? ORDER BY item")
                           [SQLInteger id]
           if null parameters
             then putStrLn "(No parameters.)"
             else mapM_ (\[SQLText parameterName] -> putStrLn parameterName)
                        parameters
           putStrLn ""
           [[SQLText body]]
             <- earlyQuery database
                           "SELECT body FROM functions WHERE id = ?"
                           [SQLInteger id]
           putStrLn ">>>>>"
           putStrLn body
           putStrLn "<<<<<"
           putStrLn ""
           putStrLn "")
        functions
  return ()

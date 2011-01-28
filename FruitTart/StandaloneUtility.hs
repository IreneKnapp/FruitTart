module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.FastCGI
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.IO

import Database.SQLite3
import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Database
import Network.FruitTart.Design
import Network.FruitTart.Types


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  
  design <- loadDesign database
  
  functionsFile <- openFile "functions.txt" WriteMode
  functions
    <- earlyQuery database
                  "SELECT id, module, name FROM functions ORDER BY module, name"
                  []
  mapM_ (\[SQLInteger id, SQLText moduleName, SQLText properName] -> do
           let headerLine = moduleName ++ "." ++ properName ++ ": " ++ show id
               headerStandoffLine
                 = replicate (length headerLine) '='
           hPutStrLn functionsFile headerStandoffLine
           hPutStrLn functionsFile headerLine
           hPutStrLn functionsFile headerStandoffLine
           hPutStrLn functionsFile ""
           actions
             <- earlyQuery database
                           ("SELECT actions.id, actions.method, "
                            ++ "controllers.mapped_name, actions.action "
                            ++ "FROM controller_actions AS actions "
                            ++ "LEFT JOIN controllers as controllers "
                            ++ "ON actions.controller = controllers.module "
                            ++ "WHERE actions.controller = ? "
                            ++ "AND actions.function = ? "
                            ++ "ORDER BY actions.method, "
                            ++ "controllers.mapped_name, actions.action")
                           [SQLText moduleName, SQLText properName]
           mapM_ (\[SQLInteger actionID, SQLText method, SQLText mappedName,
                    SQLText actionName] -> do
                    hPutStrLn functionsFile
                              $ method
                                ++ " /" ++ mappedName
                                ++ "/" ++ actionName
                    mandatoryParameters
                      <- earlyQuery database
                                    ("SELECT type "
                                     ++ "FROM action_mandatory_parameters "
                                     ++ "WHERE action = ? "
                                     ++ "ORDER BY item")
                                    [SQLInteger actionID]
                    optionalParameters
                      <- earlyQuery database
                                    ("SELECT type "
                                     ++ "FROM action_optional_parameters "
                                     ++ "WHERE action = ? "
                                     ++ "ORDER BY item")
                                    [SQLInteger actionID]
                    namedParameters
                      <- earlyQuery database
                                    ("SELECT type, name "
                                     ++ "FROM action_named_parameters "
                                     ++ "WHERE action = ? "
                                     ++ "ORDER BY name")
                                    [SQLInteger actionID]
                    let parametersLine
                          = "  " ++ (intercalate ", "
                            $ concat [map (\[SQLText typeName] -> typeName)
                                          mandatoryParameters,
                                      case map (\[SQLText typeName] -> typeName)
                                               optionalParameters of
                                        [] -> []
                                        (headType:restTypes)
                                          -> (":optional " ++ headType)
                                             : restTypes,
                                      case map (\[SQLText typeName,
                                                  SQLText name] ->
                                                  typeName ++ " " ++ name)
                                               namedParameters of
                                        [] -> []
                                        (headItem:restItems)
                                          -> (":key " ++ headItem)
                                             : restItems])
                    hPutStrLn functionsFile parametersLine
                    hPutStrLn functionsFile "")
                 actions
           parameters
             <- earlyQuery database
                           ("SELECT name FROM function_parameters "
                            ++ "WHERE function = ? ORDER BY item")
                           [SQLInteger id]
           if null parameters
             then hPutStrLn functionsFile "(No parameters.)"
             else mapM_ (\[SQLText parameterName] -> do
                           hPutStrLn functionsFile parameterName)
                        parameters
           hPutStrLn functionsFile ""
           [[SQLText body]]
             <- earlyQuery database
                           "SELECT body FROM functions WHERE id = ?"
                           [SQLInteger id]
           hPutStrLn functionsFile ">>>>>"
           hPutStrLn functionsFile body
           hPutStrLn functionsFile "<<<<<"
           catch (do
                   _ <- readExpression design moduleName body
                   return ())
                 (\e -> do
                    hPutStrLn functionsFile ""
                    hPutStrLn functionsFile
                              $ "ERROR while parsing: "
                                ++ (show (e :: SomeException)))
           hPutStrLn functionsFile ""
           hPutStrLn functionsFile "")
        functions
  hClose functionsFile
  
  templatesFile <- openFile "templates.txt" WriteMode
  templates
    <- earlyQuery database
                  "SELECT id, module, name FROM templates ORDER BY module, name"
                  []
  mapM_ (\[SQLInteger id, SQLText moduleName, SQLText properName] -> do
           let headerLine = moduleName ++ "." ++ properName ++ ": " ++ show id
               headerStandoffLine
                 = replicate (length headerLine) '='
           hPutStrLn templatesFile headerStandoffLine
           hPutStrLn templatesFile headerLine
           hPutStrLn templatesFile headerStandoffLine
           hPutStrLn templatesFile ""
           hPutStrLn templatesFile ">>>>>"
           items <- earlyQuery database
                               ("SELECT kind, body FROM template_items "
                                ++ "WHERE template = ? ORDER BY item")
                               [SQLInteger id]
           mapM_ (\[SQLText kind, SQLText body] -> do
                    let kindMarker = case kind of
                                       "content" -> '*'
                                       "expression" -> '='
                        bodyLines = case lines body of
                                  [] -> [""]
                                  [line] -> [line]
                                  bodyLines
                                    | (head $ reverse bodyLines) == ""
                                      -> reverse $ tail $ reverse bodyLines
                                    | otherwise
                                      -> bodyLines
                        markedLines = ([kindMarker] ++ head bodyLines)
                                      : (map (\line -> " " ++ line)
                                             $ tail bodyLines)
                        markedBody = unlines markedLines
                    hPutStrLn templatesFile markedBody)
                 items
           hPutStrLn templatesFile "<<<<<"
           hPutStrLn templatesFile ""
           hPutStrLn templatesFile "")
        templates
  hClose templatesFile
  
  queriesFile <- openFile "queries.txt" WriteMode
  functions
    <- earlyQuery database
                  "SELECT id, module, name FROM queries ORDER BY module, name"
                  []
  mapM_ (\[SQLInteger id, SQLText moduleName, SQLText properName] -> do
           let headerLine = moduleName ++ "." ++ properName ++ ": " ++ show id
               headerStandoffLine
                 = replicate (length headerLine) '='
           hPutStrLn queriesFile headerStandoffLine
           hPutStrLn queriesFile headerLine
           hPutStrLn queriesFile headerStandoffLine
           hPutStrLn queriesFile ""
           results
             <- earlyQuery database
                           ("SELECT type, name FROM query_results "
                            ++ "WHERE query = ? ORDER BY item")
                           [SQLInteger id]
           if null results
             then hPutStrLn queriesFile "(No results.)"
             else mapM_ (\[SQLText resultType, SQLText resultName] -> do
                           hPutStrLn queriesFile
                                     $ resultType ++ " " ++ resultName)
                        results
           hPutStrLn queriesFile ""
           [[SQLText body]]
             <- earlyQuery database
                           "SELECT body FROM queries WHERE id = ?"
                           [SQLInteger id]
           hPutStrLn queriesFile ">>>>>"
           hPutStrLn queriesFile body
           hPutStrLn queriesFile "<<<<<"
           hPutStrLn queriesFile ""
           hPutStrLn queriesFile "")
        functions
  hClose queriesFile
  

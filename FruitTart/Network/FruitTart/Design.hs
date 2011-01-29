module Network.FruitTart.Design (getDesign, loadDesign) where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Network.FastCGI
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.IO.Unsafe

import Database.SQLite3

import Network.FruitTart.Database
import Network.FruitTart.Types
import Network.FruitTart.Util
import {-# SOURCE #-} Network.FruitTart.Custard.Semantics
import Network.FruitTart.Custard.Syntax


getDesign :: FruitTart Design
getDesign = do
  FruitTartState { designMVar = designMVar } <- get
  liftIO $ readMVar designMVar


loadDesign :: Database -> IO Design
loadDesign database = do
  modules <- loadModules database
  controllers <- loadControllers database
  actions <- loadActions database
  functions <- loadFunctions database modules
  queries <- loadQueries database
  templates <- loadTemplates database modules
  let globalBindings = computeGlobalBindings functions
  return $ Design {
             designModules = modules,
             designControllers = controllers,
             designActions = actions,
             designFunctions = functions,
             designQueries = queries,
             designTemplates = templates,
             designGlobalBindings = globalBindings
           }


loadModules :: Database -> IO (Map String Module)
loadModules database = do
  rows <- earlyQuery database "SELECT name, parent FROM modules" []
  modules <- mapM (\[SQLText moduleName, maybeParentName] -> do
                     importRows <- earlyQuery database
                                              ("SELECT imported_module "
                                               ++ "FROM module_imports "
                                               ++ "WHERE importing_module = ?")
                                              [SQLText moduleName]
                     exportRows <- earlyQuery database
                                              ("SELECT symbol "
                                               ++ "FROM module_exports "
                                               ++ "WHERE module = ?")
                                              [SQLText moduleName]
                     definedRows <-
                       earlyQuery database
                                  ("SELECT name FROM functions "
                                   ++ "WHERE module = ?1 "
                                   ++ "UNION "
                                   ++ "SELECT name FROM templates "
                                   ++" WHERE module = ?1 "
                                   ++ "UNION "
                                   ++ "SELECT name FROM queries "
                                   ++ "WHERE module = ?1")
                                  [SQLText moduleName]
                     let parent = case maybeParentName of
                                    SQLNull -> Nothing
                                    SQLText parentName -> Just parentName
                         imports =
                           Set.fromList
                            $ map (\[SQLText importName] -> importName)
                                  importRows
                         exports =
                           Set.fromList
                            $ map (\[SQLText symbolName] -> symbolName)
                                  exportRows
                         definedInDatabase =
                           Set.fromList
                            $ map (\[SQLText symbolName] -> symbolName)
                                  definedRows
                         definedInBuiltinBindings =
                           Set.fromList
                            $ map snd
                                  $ filter (\(foundModuleName, _) ->
                                              moduleName == foundModuleName)
                                           $ Map.keys builtinBindings
                         defined = Set.union definedInDatabase
                                             definedInBuiltinBindings
                         module' = Module {
                             moduleParent = parent,
                             moduleImports = imports,
                             moduleExports = exports,
                             moduleDefined = defined
                           }
                     return (moduleName, module'))
                  rows
  return $ Map.fromList modules


loadControllers :: Database -> IO (Map String String)
loadControllers database = do
  mappings <- earlyQuery database
                         "SELECT mapped_name, module FROM controllers"
                         []
  return $ Map.fromList
           $ map (\[SQLText mappedName, SQLText moduleName] ->
                   (mappedName, moduleName))
                 mappings


loadActions :: Database -> IO (Map (String, String, String) Action)
loadActions database = do
  rows <- earlyQuery database
                     ("SELECT controller, action, method, function, id "
                      ++ "FROM controller_actions")
                     []
  actions <- mapM (\[SQLText controllerName,
                     SQLText actionName,
                     SQLText methodName,
                     SQLText functionName,
                     SQLInteger actionID] -> do
                         mandatoryParameterRows
                           <- earlyQuery database
                                         ("SELECT type "
                                          ++ "FROM action_mandatory_parameters "
                                          ++ "WHERE action = ? ORDER BY item")
                                         [SQLInteger actionID]
                         optionalParameterRows
                           <- earlyQuery database
                                         ("SELECT type "
                                          ++ " FROM action_optional_parameters "
                                          ++ "WHERE action = ? ORDER BY item")
                                         [SQLInteger actionID]
                         namedParameterRows
                           <- earlyQuery database
                                         ("SELECT name, type "
                                          ++ "FROM action_named_parameters "
                                          ++ "WHERE action = ?")
                                         [SQLInteger actionID]
                         let decodeType "integer" = IntegerParameter
                             decodeType "string" = StringParameter
                             decodeType _ = StringParameter
                             mandatoryParameterTypes
                               = map (\[SQLText theType] -> decodeType theType)
                                     mandatoryParameterRows
                             optionalParameterTypes
                               = map (\[SQLText theType] -> decodeType theType)
                                     optionalParameterRows
                             namedParameterTypeMap
                               = Map.fromList
                                 $ map (\[SQLText name, SQLText theType] ->
                                          (name, decodeType theType))
                                       namedParameterRows
                             action = Action {
                                        actionFunctionName = functionName,
                                        actionMandatoryParameterTypes
                                          = mandatoryParameterTypes,
                                        actionOptionalParameterTypes
                                          = optionalParameterTypes,
                                        actionNamedParameterTypeMap
                                          = namedParameterTypeMap
                                      }
                         return ((controllerName, actionName, methodName),
                                 action))
                  rows
  return $ Map.fromList actions


loadFunctions :: Database
              -> Map String Module
              -> IO (Map (String, String) Function)
loadFunctions database modules = do
  let temporaryDesign = Design {
                          designModules = modules,
                          designControllers = undefined,
                          designActions = undefined,
                          designFunctions = undefined,
                          designQueries = undefined,
                          designTemplates = undefined,
                          designGlobalBindings = undefined
                        }
  rows <- earlyQuery database "SELECT module, name, body, id FROM functions" []
  functions <- mapM (\[SQLText moduleName,
                       SQLText functionName,
                       SQLText body,
                       SQLInteger functionID] -> do
                       compiledBody
                         <- catch (readExpression temporaryDesign
                                                  moduleName
                                                  body)
                                   (\e -> error $ "While reading expression "
                                                  ++ (show body)
                                                  ++ " from body of function "
                                                  ++ moduleName
                                                  ++ "."
                                                  ++ functionName
                                                  ++ ": "
                                                  ++ (show (e :: SomeException)))
                       parameterRows <- earlyQuery
                                         database
                                         ("SELECT name "
                                          ++ "FROM function_parameters "
                                          ++ "WHERE function = ? "
                                          ++ "ORDER BY item")
                                         [SQLInteger functionID]
                       parameters <- mapM (\[SQLText parameterName] -> do
                                            TokenSymbol moduleName parameterName
                                              <- intern temporaryDesign
                                                        moduleName
                                                        parameterName
                                            return
                                             $ CustardParameter
                                                (moduleName, parameterName))
                                          parameterRows
                       let lambdaExpression =
                             CustardLambda (Just (moduleName, functionName))
                                           parameters
                                           Map.empty
                                           compiledBody
                           function = Function {
                                        functionLambdaExpression = lambdaExpression
                                      }
                       return ((moduleName, functionName), function))
                    rows
  return $ Map.fromList functions
                       


loadQueries :: Database -> IO (Map (String, String) Query)
loadQueries database = do
  rows <- earlyQuery database "SELECT module, name, body, id FROM queries" []
  queries <- mapM (\[SQLText moduleName,
                     SQLText queryName,
                     SQLText body,
                     SQLInteger queryID] -> do
                     preparedStatement <- prepare database body
                     resultRows <- earlyQuery database
                                              ("SELECT name, type "
                                               ++ "FROM query_results "
                                               ++ "WHERE query = ? "
                                               ++ "ORDER BY item")
                                              [SQLInteger queryID]
                     let valueNamesAndTypes =
                           map (\[SQLText name, SQLText typeName] ->
                                  (name,
                                   case typeName of
                                     "boolean" -> CBool
                                     "integer" -> CInt
                                     "string" -> CString
                                     "maybeInteger" -> CMaybeInt
                                     "maybeString" -> CMaybeString
                                     _ -> CInt))
                               resultRows
                         query = Query {
                                   queryPreparedStatement = preparedStatement,
                                   queryValueNamesAndTypes = valueNamesAndTypes
                                 }
                     return ((moduleName, queryName), query))
                  rows
  return $ Map.fromList queries


loadTemplates :: Database
              -> Map String Module
              -> IO (Map (String, String) Template)
loadTemplates database modules = do
  let temporaryDesign = Design {
                          designModules = modules,
                          designControllers = undefined,
                          designActions = undefined,
                          designFunctions = undefined,
                          designQueries = undefined,
                          designTemplates = undefined,
                          designGlobalBindings = undefined
                        }
  rows <- earlyQuery database "SELECT module, name, id FROM templates" []
  templates <- mapM (\[SQLText moduleName,
                       SQLText templateName,
                       SQLInteger templateID] -> do
                       itemRows <- earlyQuery database
                                              ("SELECT kind, body "
                                               ++ "FROM template_items "
                                               ++ "WHERE template = ? "
                                               ++ "ORDER BY item")
                                              [SQLInteger templateID]
                       items <- mapM (\([SQLText kind, SQLText body], index)
                                      -> do
                                        case kind of
                                          "content" -> do
                                            return $ ContentItem body
                                          "expression" -> do
                                            expression <-
                                              catch
                                               (readExpression temporaryDesign
                                                               moduleName
                                                               body)
                                               (\e -> error
                                                $ "While reading expression "
                                                  ++ (show body)
                                                  ++ " from body of template "
                                                  ++ moduleName
                                                  ++ "."
                                                  ++ templateName
                                                  ++ ", item "
                                                  ++ (show index)
                                                  ++ ": "
                                                  ++ (show
                                                      (e :: SomeException)))
                                            return $ ExpressionItem expression
                                          _ -> error
                                               $ "Unknown template item type "
                                                 ++ kind
                                                 ++ ".")
                                     $ zip itemRows [1..]
                       let template = Template {
                                        templateItems = items
                                      }
                       return ((moduleName, templateName), template))
                    rows
  return $ Map.fromList templates


computeGlobalBindings :: Map (String, String) Function
                      -> Map (String, String) CustardValue
computeGlobalBindings functions =
  Map.union (Map.map functionLambdaExpression functions)
            builtinBindings

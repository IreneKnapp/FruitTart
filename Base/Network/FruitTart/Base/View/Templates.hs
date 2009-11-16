{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, IncoherentInstances #-}
module Network.FruitTart.Base.View.Templates (
                                              -- Templates.Types
                                              TemplateValueType(..),
                                              TemplateValue(..),
                                              
                                              -- View.Templates
                                              getPageHeadItems,
                                              bind,
                                              bindQuery,
                                              bindQueryMultipleRows,
                                              bindNamedQuery,
                                              bindNamedQueryMultipleRows,
                                              namedQuery,
                                              convertRowToBindings,
                                              getBinding,
                                              unbind,
                                              clearBindings,
                                              getTemplate
                                             )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Network.FruitTart.Base
import Network.FruitTart.Base.Templates.Semantics
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


instance Bindable String where
    toTemplate string = TemplateString string
instance Bindable [Map (String, String) TemplateValue] where
    toTemplate rows = TemplateList $ map TemplateMap rows


getPageHeadItems :: FruitTart String
getPageHeadItems
    = return 
      ("<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<link href=\"/css/navigation.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<script src=\"/js/jquery.js\" type=\"text/ecmascript\"></script>\n"
       ++ "<script src=\"/js/buglist.js\" type=\"text/ecmascript\"></script>\n")


bind :: Bindable a => String -> String -> a -> FruitTart ()
bind moduleName valueName bindable = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings = Map.fromList [((moduleName, valueName), toTemplate bindable)]
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindQuery :: String -> [(String, TemplateValueType)]
          -> String -> [SQLData] -> FruitTart ()
bindQuery moduleName valueNamesAndTypes queryText queryValues = do
  [row] <- query queryText queryValues
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings = convertRowToBindings moduleName valueNamesAndTypes row
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindQueryMultipleRows :: String -> String -> [(String, TemplateValueType)]
                      -> String -> [SQLData] -> FruitTart ()
bindQueryMultipleRows moduleName overallValueName valueNamesAndTypes
                      queryText queryValues = do
  rows <- query queryText queryValues
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings
          = Map.fromList [((moduleName, overallValueName),
                           TemplateList
                           $ map (\row -> TemplateMap
                                          $ convertRowToBindings moduleName
                                                                 valueNamesAndTypes
                                                                 row)
                                 rows)]
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindNamedQuery :: String -> String -> [SQLData] -> FruitTart ()
bindNamedQuery moduleName queryName queryValues = do
  rows <- query "SELECT id, body FROM queries WHERE module = ? AND name = ?"
                 [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      bindQuery moduleName valueNamesAndTypes queryText queryValues
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


bindNamedQueryMultipleRows :: String -> String -> [SQLData] -> FruitTart ()
bindNamedQueryMultipleRows moduleName queryName queryValues = do
  rows <- query "SELECT id, body FROM queries WHERE module = ? AND name = ?"
                 [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      bindQueryMultipleRows moduleName queryName valueNamesAndTypes queryText queryValues
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


getValueNamesAndTypes :: Int64 -> FruitTart [(String, TemplateValueType)]
getValueNamesAndTypes queryID = do
  rows <- query "SELECT name, type FROM query_results WHERE query = ? ORDER BY item"
                [SQLInteger queryID]
  return $ map (\[SQLText name, SQLText typeName] ->
                 (name,
                  case typeName of
                    "boolean" -> TBool
                    "integer" -> TInt
                    "string" -> TString
                    "maybeInteger" -> TMaybeInt
                    "maybeString" -> TMaybeString
                    _ -> TInt))
               rows


namedQuery :: String -> String -> [SQLData]
           -> FruitTart [Map (String, String) TemplateValue]
namedQuery moduleName queryName queryValues = do
  rows <- query "SELECT id, body FROM queries WHERE module = ? AND name = ?"
                 [SQLText moduleName, SQLText queryName]
  case rows of
    [[SQLInteger queryID, SQLText queryText]] -> do
      valueNamesAndTypes <- getValueNamesAndTypes queryID
      rows <- query queryText queryValues
      return $ map (\row -> convertRowToBindings moduleName
                                                 valueNamesAndTypes
                                                 row)
                   rows
    _ -> error $ "Query " ++ moduleName ++ "." ++ queryName ++ " not found."


convertRowToBindings :: String -> [(String, TemplateValueType)] -> [SQLData]
                     -> Map (String, String) TemplateValue
convertRowToBindings moduleName valueNamesAndTypes row
    = if length valueNamesAndTypes /= length row
        then error $ "Provided with " ++ (show $ length valueNamesAndTypes)
                   ++ " value names and types, but " ++ (show $ length row)
                   ++ " values."
        else
           Map.fromList
           $ map (\((columnName, valueType), value) ->
                      ((moduleName, columnName),
                       case valueType of
                         TBool -> case value of
                                    SQLInteger integer -> TemplateBool $ integer /= 0
                                    _ -> error "Value from query not an integer."
                         TInt -> case value of
                                   SQLInteger integer -> TemplateInteger integer
                                   _ -> error "Value from query not an integer."
                         TString -> case value of
                                      SQLText string -> TemplateString string
                                      _ -> error "Value from query not a string."
                         TMaybeInt -> case value of
                                        SQLNull -> TemplateMaybe Nothing
                                        SQLInteger integer -> TemplateMaybe
                                                              $ Just
                                                              $ TemplateInteger integer
                                        _ -> error
                                             "Value from query not an integer or null."
                         TMaybeString -> case value of
                                        SQLNull -> TemplateMaybe Nothing
                                        SQLText string -> TemplateMaybe
                                                              $ Just
                                                              $ TemplateString string
                                        _ -> error
                                             "Value from query not a string or null."))
                 $ zip valueNamesAndTypes row


getBinding :: String -> String -> FruitTart (Maybe TemplateValue)
getBinding moduleName valueName = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  bindings <- liftIO $ readMVar bindingsMVar
  return $ Map.lookup (moduleName, valueName) bindings


unbind :: String -> String -> FruitTart ()
unbind moduleName valueName = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let bindings' = Map.delete (moduleName, valueName) oldBindings
  liftIO $ putMVar bindingsMVar bindings'


clearBindings :: FruitTart ()
clearBindings = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  liftIO $ takeMVar bindingsMVar
  liftIO $ putMVar bindingsMVar baseBindings


getTemplate :: String -> String -> FruitTart String
getTemplate moduleName templateName = do
  bindingsMVar <- getInterfaceStateMVar "Base"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  bindings <- liftIO $ readMVar bindingsMVar
  fillTemplate moduleName templateName bindings

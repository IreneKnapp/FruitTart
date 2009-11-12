module Network.FruitTart.Templates.View.Templates (
                                                   functionTable,
                                                   bind,
                                                   bindQuery,
                                                   bindQueryMultipleRows,
                                                   convertRowToBindings,
                                                   getTemplate
                                                  )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Network.FruitTart.Base
import Network.FruitTart.PluginInterface
import Network.FruitTart.Templates.Imports
import Network.FruitTart.Templates.Semantics
import Network.FruitTart.Templates.Types
import Network.FruitTart.Util


functionTable :: FunctionTable
functionTable
    = makeFunctionTable [("bind", toDyn bind'),
                         ("bindQuery", toDyn bindQuery),
                         ("bindQueryMultipleRows", toDyn bindQueryMultipleRows),
                         ("convertRowToBindings", toDyn convertRowToBindings),
                         ("getTemplate", toDyn getTemplate)]


bind :: Bindable a => String -> String -> a -> FruitTart ()
bind moduleName valueName bindable = bind' moduleName valueName $ AnyBindable bindable


bind' :: String -> String -> AnyBindable -> FruitTart ()
bind' moduleName valueName (AnyBindable bindable) = do
  bindingsMVar <- getInterfaceStateMVar "Templates"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  oldBindings <- liftIO $ takeMVar bindingsMVar
  let newBindings = Map.fromList [((moduleName, valueName), toTemplate bindable)]
      bindings' = Map.union newBindings oldBindings
  liftIO $ putMVar bindingsMVar bindings'


bindQuery :: String -> [(String, TemplateValueType)]
          -> String -> [SQLData] -> FruitTart ()
bindQuery moduleName valueNamesAndTypes queryText queryValues = do
  [row] <- query queryText queryValues
  bindingsMVar <- getInterfaceStateMVar "Templates"
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
  bindingsMVar <- getInterfaceStateMVar "Templates"
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
                                      _ -> error "Value from query not a string."))
                 $ zip valueNamesAndTypes row


getTemplate :: String -> String -> FruitTart String
getTemplate moduleName templateName = do
  bindingsMVar <- getInterfaceStateMVar "Templates"
               :: FruitTart (MVar (Map (String, String) TemplateValue))
  bindings <- liftIO $ readMVar bindingsMVar
  fillTemplate moduleName templateName bindings []

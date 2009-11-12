module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import System.Exit
import Database.SQLite3

import Network.FruitTart.Templates.Imports
import qualified Network.FruitTart.Templates.Controller.Templates
    as Controller.Templates
import Network.FruitTart.Templates.Types
import qualified Network.FruitTart.Templates.View.Templates
    as View.Templates
import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceFunctionTable = functionTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceImportFunctionTableMVar = importFunctionTableMVar
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("templates", Controller.Templates.actionTable)]


functionTable :: CombinedFunctionTable
functionTable
    = combineFunctionTables
      [("Templates.Controller.Templates", Controller.Templates.functionTable),
       ("Templates.View.Templates", View.Templates.functionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Templates"


moduleVersion :: Int64
moduleVersion = 1


moduleSchemaVersion :: Int64
moduleSchemaVersion = 1


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES (?, ?)"
             [SQLText moduleName, SQLInteger moduleSchemaVersion]
  earlyQuery database
             (  "CREATE TABLE templates (\n"
             ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
             ++ "module TEXT,\n"
             ++ "name TEXT,\n"
             ++ "CONSTRAINT key UNIQUE (module, name)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE template_items (\n"
             ++ "template INTEGER,\n"
             ++ "item INTEGER,\n"
             ++ "kind TEXT,\n"
             ++ "body TEXT,\n"
             ++ "CONSTRAINT key PRIMARY KEY (template, item)\n"
             ++ ")")
             []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newMVar (Map.empty :: Map (String, String) TemplateValue)
  return $ toDyn mVar

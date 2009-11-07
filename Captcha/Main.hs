module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import System.Environment
import System.Exit
import Database.SQLite3

import Network.FruitTart.Captcha.Imports
import qualified Network.FruitTart.Captcha.Controller.Captcha
    as Controller.Captcha
import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceFunctionTable = functionTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceImportFunctionTableMVar = importFunctionTableMVar
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("captcha", Controller.Captcha.actionTable)]


functionTable :: CombinedFunctionTable
functionTable
    = combineFunctionTables
      [("Captcha.Controller.Captcha", Controller.Captcha.functionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Captcha"


moduleSchemaVersion :: Int64
moduleSchemaVersion = 1


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES (?, ?)"
             [SQLText moduleName, SQLInteger moduleSchemaVersion]
  return ()

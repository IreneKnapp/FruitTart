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

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables []


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Navigation"


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
             (  "CREATE TABLE navigation_items (\n"
             ++ "id INTEGER PRIMARY KEY,\n"
             ++ "name TEXT,\n"
             ++ "link TEXT,\n"
             ++ "within_managed_tree INTEGER,\n"
             ++ "separator INTEGER,\n"
             ++ "always_enabled INTEGER,\n"
             ++ "class TEXT\n"
             ++ ")")
             []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newEmptyMVar :: IO (MVar String)
  return $ toDyn mVar

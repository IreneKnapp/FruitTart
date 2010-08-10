module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import System.Environment
import System.Exit
import Database.SQLite3

import qualified Network.FruitTart.Adventure.Controller.Adventure
    as Controller.Adventure
import Network.FruitTart.Util


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1),
                                              ("Base", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceInitRequest = initRequest
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("adventure", Controller.Adventure.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Adventure"


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
             (  "INSERT INTO module_imports "
             ++ "(importing_module, imported_module) "
             ++ "VALUES ('Adventure', 'Base')")
             []
  earlyQuery database
                 (  "CREATE TABLE adventure_users (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "right_edit INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER adventure_insert_users\n"
                 ++ "AFTER INSERT ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "INSERT INTO adventure_users\n"
                 ++ "(id, right_edit)\n"
                 ++ "VALUES (NEW.id, 0);\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER adventure_update_users\n"
                 ++ "AFTER UPDATE OF id ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "UPDATE adventure_users SET id = NEW.id WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER adventure_delete_users\n"
                 ++ "AFTER DELETE ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "DELETE FROM adventure_users WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "INSERT INTO adventure_users\n"
                 ++ "(id, right_edit)\n"
                 ++ "SELECT id, 0 FROM users")
                 []
  earlyQuery database
                 (  "UPDATE adventure_users\n"
                 ++ "SET right_edit = 1\n"
                 ++ "WHERE id = (SELECT owner_user FROM settings)")
                 []
  earlyQuery database
                 (  "CREATE TABLE adventure_nodes (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "name TEXT,\n"
                 ++ "body TEXT\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE adventure_options (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "parent INTEGER,\n"
                 ++ "item INTEGER,\n"
                 ++ "child INTEGER,\n"
                 ++ "name TEXT,\n"
                 ++ "CONSTRAINT parent_item_unique PRIMARY KEY (parent, item)\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE adventure_variables (\n"
                 ++ "name TEXT PRIMARY KEY\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE adventure_option_variable_effects (\n"
                 ++ "option INTEGER,\n"
                 ++ "variable TEXT,\n"
                 ++ "effect INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
             (  "INSERT INTO stylesheets (name, priority, controller)\n"
             ++ "VALUES ('adventure.css', 100, 'Adventure');")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('adventure.js', 100, 'Adventure');")
             []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newEmptyMVar :: IO (MVar String)
  return $ toDyn mVar


initRequest :: FruitTart ()
initRequest = return ()

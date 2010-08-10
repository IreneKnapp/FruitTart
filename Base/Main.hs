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

import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util
import qualified Network.FruitTart.Base.Controller.Functions as Controller.Functions
import qualified Network.FruitTart.Base.Controller.Login as Controller.Login
import qualified Network.FruitTart.Base.Controller.Templates as Controller.Templates
import qualified Network.FruitTart.Base.Controller.Queries as Controller.Queries
import qualified Network.FruitTart.Base.View.Templates as View.Templates


fruitTartPlugin :: Dynamic
fruitTartPlugin = toDyn $ Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceInitRequest = initRequest
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("functions", Controller.Functions.actionTable),
       ("login", Controller.Login.actionTable),
       ("templates", Controller.Templates.actionTable),
       ("queries", Controller.Queries.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Base"


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
             (  "CREATE TABLE base_users (\n"
             ++ "id INTEGER PRIMARY KEY,\n"
             ++ "right_admin_design INTEGER\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TRIGGER base_insert_users\n"
             ++ "AFTER INSERT ON users\n"
             ++ "FOR EACH ROW BEGIN\n"
             ++ "INSERT INTO base_users\n"
             ++ "(id, right_admin_design)\n"
             ++ "VALUES (NEW.id, 0);\n"
             ++ "END;")
             []
  earlyQuery database
             (  "CREATE TRIGGER base_update_users\n"
             ++ "AFTER UPDATE OF id ON users\n"
             ++ "FOR EACH ROW BEGIN\n"
             ++ "UPDATE base_users SET id = NEW.id WHERE id = OLD.id;\n"
             ++ "END;")
             []
  earlyQuery database
             (  "CREATE TRIGGER base_delete_users\n"
             ++ "AFTER DELETE ON users\n"
             ++ "FOR EACH ROW BEGIN\n"
             ++ "DELETE FROM base_users WHERE id = OLD.id;\n"
             ++ "END;")
             []
  earlyQuery database
             (  "INSERT INTO base_users\n"
             ++ "(id, right_admin_design)\n"
             ++ "SELECT id, 0 FROM users")
             []
  earlyQuery database
             (  "CREATE TABLE navigation_items (\n"
             ++ "id INTEGER PRIMARY KEY,\n"
             ++ "name TEXT,\n"
             ++ "link TEXT,\n"
             ++ "within_managed_tree INTEGER,\n"
             ++ "separator INTEGER,\n"
             ++ "always_enabled INTEGER,\n"
             ++ "name_is_html INTEGER,\n"
             ++ "class TEXT\n"
             ++ ")")
             []
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
  earlyQuery database
             (  "CREATE TABLE queries (\n"
             ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
             ++ "module TEXT,\n"
             ++ "name TEXT,\n"
             ++ "is_template_expression INTEGER,\n"
             ++ "body TEXT,\n"
             ++ "CONSTRAINT key UNIQUE (module, name)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE query_results (\n"
             ++ "query INTEGER,\n"
             ++ "item INTEGER,\n"
             ++ "type TEXT,\n"
             ++ "name TEXT,\n"
             ++ "CONSTRAINT key PRIMARY KEY (query, item)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE functions (\n"
             ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
             ++ "module TEXT,\n"
             ++ "name TEXT,\n"
             ++ "body TEXT,\n"
             ++ "CONSTRAINT key UNIQUE (module, name)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE function_parameters (\n"
             ++ "function INTEGER,\n"
             ++ "item INTEGER,\n"
             ++ "name TEXT,\n"
             ++ "CONSTRAINT key PRIMARY KEY (function, item)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE module_imports ("
             ++ "importing_module TEXT,"
             ++ "imported_module TEXT,"
             ++ "CONSTRAINT key UNIQUE (importing_module, imported_module)"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO module_imports "
             ++ "(importing_module, imported_module) "
             ++ "VALUES ('Base.Functions', 'Base')")
             []
  earlyQuery database
             (  "INSERT INTO module_imports "
             ++ "(importing_module, imported_module) "
             ++ "VALUES ('Base.Login', 'Base')")
             []
  earlyQuery database
             (  "INSERT INTO module_imports "
             ++ "(importing_module, imported_module) "
             ++ "VALUES ('Base.Queries', 'Base')")
             []
  earlyQuery database
             (  "INSERT INTO module_imports "
             ++ "(importing_module, imported_module) "
             ++ "VALUES ('Base.Templates', 'Base')")
             []
  earlyQuery database
             (  "CREATE TABLE stylesheets (\n"
             ++ "name TEXT,\n"
             ++ "priority INTEGER,\n"
             ++ "controller TEXT\n"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO stylesheets (name, priority, controller)\n"
             ++ "VALUES ('normal.css', 1, '%');")
             []
  earlyQuery database
             (  "INSERT INTO stylesheets (name, priority, controller)\n"
             ++ "VALUES ('navigation.css', 2, '%');")
             []
  earlyQuery database
             (  "INSERT INTO stylesheets (name, priority, controller)\n"
             ++ "VALUES ('base.css', 100, 'Base.%');")
             []
  earlyQuery database
             (  "CREATE TABLE scripts (\n"
             ++ "name TEXT,\n"
             ++ "priority INTEGER,\n"
             ++ "controller TEXT\n"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('jquery.js', 1, '%');")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('fruit-tart.js', 2, '%');")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('xhtml-document-write.js', 3, '%');")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('queries.js', 100, 'Base.Queries');")
             []
  earlyQuery database
             (  "INSERT INTO scripts (name, priority, controller)\n"
             ++ "VALUES ('templates.js', 100, 'Base.Templates');")
             []
  earlyQuery database
             (  "CREATE TABLE week_day_names (\n"
             ++ "number INTEGER PRIMARY KEY,\n"
             ++ "name TEXT\n"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (0, 'Sunday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (1, 'Monday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (2, 'Tuesday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (3, 'Wednesday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (4, 'Thursday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (5, 'Friday')")
             []
  earlyQuery database
             (  "INSERT INTO week_day_names (number, name) VALUES (6, 'Saturday')")
             []
  earlyQuery database
             (  "CREATE TABLE month_names (\n"
             ++ "number INTEGER PRIMARY KEY,\n"
             ++ "name TEXT\n"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (1, 'January')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (2, 'February')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (3, 'March')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (4, 'April')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (5, 'May')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (6, 'June')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (7, 'July')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (8, 'August')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (9, 'September')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (10, 'October')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (11, 'November')")
             []
  earlyQuery database
             (  "INSERT INTO month_names (number, name) VALUES (12, 'December')")
             []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newMVar (Map.empty :: Map (String, String) TemplateValue)
  return $ toDyn mVar


initRequest :: FruitTart ()
initRequest = do
  View.Templates.clearBindings

module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Network.FastCGI hiding (logCGI)
import Network.CGI.Monad
import System.Environment
import System.Exit

import qualified Network.FruitTart.Buglist.Controller.Issues
    as Controller.Issues
import qualified Network.FruitTart.Buglist.Controller.Users
    as Controller.Users
import qualified Network.FruitTart.Buglist.Controller.Synchronization
    as Controller.Synchronization
import Network.FruitTart.PluginInterface
import qualified Database.SQLite3 as SQL


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    dispatchTable = dispatchTable,
                    initDatabase = initDatabase
                  }


dispatchTable :: ControllerTable
dispatchTable
    = Map.fromList
      [("issues", Network.FruitTart.Controller.Issues.actionTable),
       ("users", Network.FruitTart.Controller.Users.actionTable),
       ("synchronization", Network.FruitTart.Controller.Synchronization.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Buglist"


moduleSchemaVersion :: Int64
moduleSchemaVersion = 1


initDatabase :: SQL.Database -> IO ()
initDatabase database = do
  [[SQLInteger schemaVersionCount]]
      <- earlyQuery database
                    "SELECT count(*) FROM schema_versions WHERE module = 'Buglist'"
                    []
  if schemaVersionCount == SQLInteger 0
     then do
       earlyRun' database
            "INSERT INTO schema_versions (module, version) VALUES ('Buglist', ?)"
            [SQLInteger moduleSchemaVersion]
       initDatabase' database
     else do
       SQLInteger databaseSchemaVersion
           <- earlyEval database
                        "SELECT version FROM schema_version WHERE module = 'Buglist'"
       if databaseSchemaVersion /= schemaVersion
          then do
            logCGI $ "Schema mismatch: Program version " ++ (show schemaVersion)
                     ++ ", database version " ++ (show databaseSchemaVersion)
                     ++ "."
            exitFailure
          else do initDatabase' database


initDatabase' :: SQL.Database -> IO ()
initDatabase' database = do
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "issue_index_filter_which TEXT,\n"
                 ++ "issue_index_filter_all_modules INTEGER,\n"
                 ++ "issue_index_filter_module INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER IF NOT EXISTS buglist_insert_sessions (\n"
                 ++ "AFTER INSERT ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "INSERT INTO buglist_sessions (id) VALUES (NEW.id);\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER IF NOT EXISTS buglist_update_sessions (\n"
                 ++ "AFTER UPDATE OF id ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "UPDATE buglist_sessions SET id = NEW.id WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER IF NOT EXISTS buglist_delete_sessions (\n"
                 ++ "AFTER DELETE ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "DELETE FROM buglist_sessions WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_issues (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "status INTEGER,\n"
                 ++ "resolution INTEGER,\n"
                 ++ "module INTEGER,\n"
                 ++ "severity INTEGER,\n"
                 ++ "priority INTEGER,\n"
                 ++ "assignee INTEGER,\n"
                 ++ "reporter INTEGER,\n"
                 ++ "summary TEXT,\n"
                 ++ "timestamp_created INTEGER,\n"
                 ++ "timestamp_modified INTEGER,\n"
                 ++ "CONSTRAINT key UNIQUE (reporter, timestamp_created)\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_statuses (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger statusCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_statuses" []
  if statusCount == 0
     then do
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (1, 'NEW')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (2, 'REOPENED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (3, 'UNCONFIRMED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (4, 'CONFIRMED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (5, 'ASSIGNED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (6, 'RESOLVED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_statuses (id, name) VALUES (7, 'CLOSED')"
                  []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_resolutions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger resolutionCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_resolutions" []
  if resolutionCount == 0
     then do
       earlyQuery database
                  "INSERT INTO buglist_resolutions (id, name) VALUES (1, '---')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_resolutions (id, name) VALUES (2, 'FIXED')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_resolutions (id, name) VALUES (3, 'WONTFIX')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_resolutions (id, name) VALUES (4, 'WORKSFORME')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_resolutions (id, name) VALUES (5, 'DUPLICATE')"
                  []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_modules (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger moduleCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_modules" []
  if moduleCount == 0
     then do
       earlyQuery database
                  "INSERT INTO buglist_modules (id, name) VALUES (1, 'Program')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_modules (id, name) VALUES (2, 'Documentation')"
                  []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_severities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger severityCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_severities" []
  if severityCount == 0
     then do
       earlyQuery database
                  "INSERT INTO buglist_severities (id, name) VALUES (1, 'critical')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_severities (id, name) VALUES (2, 'major')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_severities (id, name) VALUES (3, 'normal')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_severities (id, name) VALUES (4, 'minor')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_severities (id, name) VALUES (5, 'enhancement')"
                  []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_priorities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger priorityCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_priorities" []
  if priorityCount == 0
     then do
       earlyQuery database
                  "INSERT INTO buglist_priorities (id, name) VALUES (1, 'high')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_priorities (id, name) VALUES (2, 'normal')"
                  []
       earlyQuery database
                  "INSERT INTO buglist_priorities (id, name) VALUES (3, 'low')"
                  []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_issue_defaults (\n"
                 ++ "status INTEGER,\n"
                 ++ "resolution INTEGER,\n"
                 ++ "severity INTEGER,\n"
                 ++ "priority INTEGER\n"
                 ++ ")")
                 []
  [[SQLInteger defaultsCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_issue_defaults" []
  if defaultsCount == 0
     then do
       earlyQuery database
                     (  "INSERT INTO buglist_issue_defaults "
                     ++ "(status, resolution, severity, priority) "
                     ++ "VALUES (1, 1, 3, 2)")
                     []
     else return ()
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_user_issue_changes (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "status_changed INTEGER,\n"
                 ++ "resolution_changed INTEGER,\n"
                 ++ "module_changed INTEGER,\n"
                 ++ "severity_changed INTEGER,\n"
                 ++ "priority_changed INTEGER,\n"
                 ++ "assignee_changed INTEGER,\n"
                 ++ "summary_changed INTEGER,\n"
                 ++ "old_status INTEGER,\n"
                 ++ "old_resolution INTEGER,\n"
                 ++ "old_module INTEGER,\n"
                 ++ "old_severity INTEGER,\n"
                 ++ "old_priority INTEGER,\n"
                 ++ "old_assignee INTEGER,\n"
                 ++ "old_summary TEXT,\n"
                 ++ "new_status INTEGER,\n"
                 ++ "new_resolution INTEGER,\n"
                 ++ "new_module INTEGER,\n"
                 ++ "new_severity INTEGER,\n"
                 ++ "new_priority INTEGER,\n"
                 ++ "new_assignee INTEGER,\n"
                 ++ "new_summary TEXT,\n"
                 ++ "CONSTRAINT key PRIMARY KEY (user, issue, timestamp)\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_user_issue_comments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "text TEXT,\n"
                 ++ "CONSTRAINT key PRIMARY KEY (user, issue, timestamp)\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_user_issue_attachments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "filename TEXT,\n"
                 ++ "data BLOB,\n"
                 ++ "CONSTRAINT key1 PRIMARY KEY (user, issue, timestamp),\n"
                 ++ "CONSTRAINT key2 UNIQUE (issue, filename)\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS buglist_navigation_items (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT,\n"
                 ++ "link TEXT,\n"
                 ++ "within_buglist_tree INTEGER,\n"
                 ++ "separator INTEGER,\n"
                 ++ "always_enabled INTEGER,\n"
                 ++ "class TEXT\n"
                 ++ ")")
                 []
  [[SQLInteger navigationItemCount]]
      <- earlyQuery database "SELECT count(*) FROM buglist_navigation_items" []
  if navigationItemCount == 0
     then do
       earlyQuery database
                  (  "INSERT INTO buglist_navigation_items "
                  ++ "(id, name, link, within_buglist_tree, separator, "
                  ++ "always_enabled, class) "
                  ++ "VALUES (1, 'Report an Issue', '/issues/create/', 1, 0, 0, NULL)")
                  []
       earlyQuery database
                  (  "INSERT INTO buglist_navigation_items "
                  ++ "(id, name, link, within_buglist_tree, separator, "
                  ++ "always_enabled, class) "
                  ++ "VALUES (2, 'Issue List', '/issues/index/', 1, 0, 0, NULL)")
                  []
       earlyQuery database
                  (  "INSERT INTO buglist_navigation_items "
                  ++ "(id, name, link, within_buglist_tree, separator, "
                  ++ "always_enabled, class) "
                  ++ "VALUES (3, 'User List', '/users/index/', 1, 0, 0, NULL)")
                  []
     else return ()

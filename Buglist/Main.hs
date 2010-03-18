module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import System.Environment
import System.Exit
import Database.SQLite3

import qualified Network.FruitTart.Buglist.Controller.Issues
    as Controller.Issues
import qualified Network.FruitTart.Buglist.Controller.Users
    as Controller.Users
import qualified Network.FruitTart.Buglist.Controller.Synchronization
    as Controller.Synchronization
import qualified Network.FruitTart.Buglist.View.Navigation
    as View.Navigation
import Network.FruitTart.Util


fruitTartPlugin :: Dynamic
fruitTartPlugin = toDyn $ Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("Base", 1),
                                              ("Captcha", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceInitRequest = initRequest
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("issues", Controller.Issues.actionTable),
       ("users", Controller.Users.actionTable),
       ("synchronization", Controller.Synchronization.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Buglist"


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
                 (  "CREATE TABLE buglist_sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "issue_index_filter_which TEXT,\n"
                 ++ "issue_index_filter_all_modules INTEGER,\n"
                 ++ "issue_index_filter_module INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_insert_sessions\n"
                 ++ "AFTER INSERT ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "INSERT INTO buglist_sessions (id) VALUES (NEW.id);\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_update_sessions\n"
                 ++ "AFTER UPDATE OF id ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "UPDATE buglist_sessions SET id = NEW.id WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_delete_sessions\n"
                 ++ "AFTER DELETE ON sessions\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "DELETE FROM buglist_sessions WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TABLE buglist_users (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "right_synchronize INTEGER,\n"
                 ++ "right_admin_users INTEGER,\n"
                 ++ "right_see_emails INTEGER,\n"
                 ++ "right_report_issues INTEGER,\n"
                 ++ "right_modify_issues INTEGER,\n"
                 ++ "right_upload_files INTEGER,\n"
                 ++ "right_comment_issues INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_insert_users\n"
                 ++ "AFTER INSERT ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "INSERT INTO buglist_users\n"
                 ++ "(id, right_synchronize, right_admin_users, right_see_emails,\n"
                 ++ "right_report_issues, right_modify_issues, right_upload_files,\n"
                 ++ "right_comment_issues)\n"
                 ++ "VALUES (NEW.id, 0, 0, 0, 1, 0, 0, 1);\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_update_users\n"
                 ++ "AFTER UPDATE OF id ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "UPDATE buglist_users SET id = NEW.id WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "CREATE TRIGGER buglist_delete_users\n"
                 ++ "AFTER DELETE ON users\n"
                 ++ "FOR EACH ROW BEGIN\n"
                 ++ "DELETE FROM buglist_users WHERE id = OLD.id;\n"
                 ++ "END;")
                 []
  earlyQuery database
                 (  "INSERT INTO buglist_users\n"
                 ++ "(id, right_synchronize, right_admin_users, right_see_emails,\n"
                 ++ "right_report_issues, right_modify_issues, right_upload_files,\n"
                 ++ "right_comment_issues)\n"
                 ++ "SELECT id, 0, 0, 0, 1, 0, 0, 1 FROM users")
                 []
  earlyQuery database
                 (  "UPDATE buglist_users\n"
                 ++ "SET right_synchronize = 1, right_admin_users = 1,\n"
                 ++ "right_see_emails = 1, right_report_issues = 1,\n"
                 ++ "right_modify_issues = 1, right_upload_files = 1,\n"
                 ++ "right_comment_issues = 1\n"
                 ++ "WHERE id = (SELECT owner_user FROM settings)")
                 []
  earlyQuery database
                 (  "UPDATE buglist_users\n"
                 ++ "SET right_synchronize = 0, right_admin_users = 0,\n"
                 ++ "right_see_emails = 0, right_report_issues = 1,\n"
                 ++ "right_modify_issues = 0, right_upload_files = 0,\n"
                 ++ "right_comment_issues = 1\n"
                 ++ "WHERE id = (SELECT anonymous_user FROM settings)")
                 []
  earlyQuery database
                 (  "UPDATE buglist_users\n"
                 ++ "SET right_synchronize = 0, right_admin_users = 0,\n"
                 ++ "right_see_emails = 0, right_report_issues = 0,\n"
                 ++ "right_modify_issues = 0, right_upload_files = 0,\n"
                 ++ "right_comment_issues = 0\n"
                 ++ "WHERE id = (SELECT nobody_user FROM settings)")
                 []
  earlyQuery database
                 (  "CREATE TABLE buglist_issues (\n"
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
                 (  "CREATE TABLE buglist_statuses (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
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
  earlyQuery database
                 (  "CREATE TABLE buglist_resolutions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
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
  earlyQuery database
                 (  "CREATE TABLE buglist_modules (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "sort_order INTEGER UNIQUE,\n"
                 ++ "name TEXT,\n"
                 ++ "visible_only_when_logged_in INTEGER\n"
                 ++ ")")
                 []
  earlyQuery database
             "INSERT INTO buglist_modules (id, name) VALUES (1, 'Program')"
             []
  earlyQuery database
             "INSERT INTO buglist_modules (id, name) VALUES (2, 'Documentation')"
             []
  earlyQuery database
                 (  "CREATE TABLE buglist_severities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")")
                 []
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
  earlyQuery database
             (  "CREATE TABLE buglist_priorities (\n"
             ++ "id INTEGER PRIMARY KEY,\n"
             ++ "name TEXT\n"
             ++ ")")
             []
  earlyQuery database
             "INSERT INTO buglist_priorities (id, name) VALUES (1, 'high')"
             []
  earlyQuery database
             "INSERT INTO buglist_priorities (id, name) VALUES (2, 'normal')"
             []
  earlyQuery database
             "INSERT INTO buglist_priorities (id, name) VALUES (3, 'low')"
             []
  earlyQuery database
             (  "CREATE TABLE buglist_issue_defaults (\n"
             ++ "status INTEGER,\n"
             ++ "resolution INTEGER,\n"
             ++ "severity INTEGER,\n"
             ++ "priority INTEGER\n"
             ++ ")")
             []
  earlyQuery database
             (  "INSERT INTO buglist_issue_defaults "
             ++ "(status, resolution, severity, priority) "
             ++ "VALUES (1, 1, 3, 2)")
             []
  earlyQuery database
             (  "CREATE TABLE buglist_user_issue_changes (\n"
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
             (  "CREATE TABLE buglist_user_issue_comments (\n"
             ++ "user INTEGER,\n"
             ++ "issue INTEGER,\n"
             ++ "timestamp INTEGER,\n"
             ++ "text TEXT,\n"
             ++ "CONSTRAINT key PRIMARY KEY (user, issue, timestamp)\n"
             ++ ")")
             []
  earlyQuery database
             (  "CREATE TABLE buglist_user_issue_attachments (\n"
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
             (  "INSERT INTO navigation_items "
             ++ "(id, name, link, within_buglist_tree, separator, "
             ++ "always_enabled, name_is_html, class) "
             ++ "VALUES (101, 'Report an Issue', '/issues/create/', 1, 0, 0, 0, NULL)")
             []
  earlyQuery database
             (  "INSERT INTO navigation_items "
             ++ "(id, name, link, within_buglist_tree, separator, "
             ++ "always_enabled, name_is_html, class) "
             ++ "VALUES (102, 'Issue List', '/issues/index/', 1, 0, 0, 0, NULL)")
             []
  earlyQuery database
             (  "INSERT INTO navigation_items "
             ++ "(id, name, link, within_buglist_tree, separator, "
             ++ "always_enabled, name_is_html, class) "
             ++ "VALUES (103, 'User List', '/users/index/', 1, 0, 0, 0 NULL)")
             []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newEmptyMVar :: IO (MVar String)
  return $ toDyn mVar


initRequest :: FruitTart ()
initRequest = return ()

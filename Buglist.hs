module Buglist where

import Control.Monad.State
import Network.FastCGI
import Network.CGI.Monad
import System.Environment

import qualified Dispatcher
import Passwords
import qualified SQLite3 as SQL
import Types


main :: IO ()
main = do
  databasePath <- getEnv "BUGLIST_DB"
  database <- SQL.open databasePath
  initDatabase database
  state <- return $ BuglistState {
             database = database
           }
  runFastCGIorCGI $ evalStateT Dispatcher.processRequest state
  return ()


initDatabase :: SQL.Database -> IO ()
initDatabase database = do
  run database $ "CREATE TABLE IF NOT EXISTS issues (\n"
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
                 ++ "CONSTRAINT key UNIQUE (reporter, timestamp_created)"
                 ++ ")"
  run database $ "CREATE TABLE IF NOT EXISTS statuses (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  statusCount <- eval database "SELECT count(*) FROM statuses"
  if statusCount == SQL.SQLInteger 0
     then do
       run database $ "INSERT INTO statuses (id, name) VALUES (1, 'NEW')"
       run database $ "INSERT INTO statuses (id, name) VALUES (2, 'REOPENED')"
       run database $ "INSERT INTO statuses (id, name) VALUES (3, 'UNCONFIRMED')"
       run database $ "INSERT INTO statuses (id, name) VALUES (4, 'CONFIRMED')"
       run database $ "INSERT INTO statuses (id, name) VALUES (5, 'ASSIGNED')"
       run database $ "INSERT INTO statuses (id, name) VALUES (6, 'RESOLVED')"
       run database $ "INSERT INTO statuses (id, name) VALUES (7, 'CLOSED')"
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS resolutions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  resolutionCount <- eval database "SELECT count(*) FROM resolutions"
  if resolutionCount == SQL.SQLInteger 0
     then do
       run database $ "INSERT INTO resolutions (id, name) VALUES (1, '---')"
       run database $ "INSERT INTO resolutions (id, name) VALUES (2, 'FIXED')"
       run database $ "INSERT INTO resolutions (id, name) VALUES (3, 'WONTFIX')"
       run database $ "INSERT INTO resolutions (id, name) VALUES (4, 'WORKSFORME')"
       run database $ "INSERT INTO resolutions (id, name) VALUES (5, 'DUPLICATE')"
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS modules (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  moduleCount <- eval database "SELECT count(*) FROM modules"
  if moduleCount == SQL.SQLInteger 0
     then do
       run database $ "INSERT INTO modules (id, name) VALUES (1, 'Program')"
       run database $ "INSERT INTO modules (id, name) VALUES (2, 'Documentation')"
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS severities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  severityCount <- eval database "SELECT count(*) FROM severities"
  if severityCount == SQL.SQLInteger 0
     then do
       run database $ "INSERT INTO severities (id, name) VALUES (1, 'critical')"
       run database $ "INSERT INTO severities (id, name) VALUES (2, 'major')"
       run database $ "INSERT INTO severities (id, name) VALUES (3, 'normal')"
       run database $ "INSERT INTO severities (id, name) VALUES (4, 'minor')"
       run database $ "INSERT INTO severities (id, name) VALUES (5, 'enhancement')"
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS priorities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  priorityCount <- eval database "SELECT count(*) FROM priorities"
  if priorityCount == SQL.SQLInteger 0
     then do
       run database $ "INSERT INTO priorities (id, name) VALUES (1, 'high')"
       run database $ "INSERT INTO priorities (id, name) VALUES (2, 'normal')"
       run database $ "INSERT INTO priorities (id, name) VALUES (3, 'low')"
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS defaults (\n"
                 ++ "status INTEGER,\n"
                 ++ "resolution INTEGER,\n"
                 ++ "severity INTEGER,\n"
                 ++ "priority INTEGER\n"
                 ++ ")"
  defaultsCount <- evalDatabase "SELECT count(*) FROM defaults"
  if defaultsCount == SQL.SQLInteger 0
     then do
       run database ("INSERT INTO defaults (status, resolution, severity, priority) "
                     "VALUES (1, 1, 3, 2)")
  run database $ "CREATE TABLE IF NOT EXISTS users (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "full_name TEXT,\n"
                 ++ "email TEXT,\n"
                 ++ "password_hash BLOB,\n"
                 ++ "right_admin_users INTEGER,\n"
                 ++ "right_see_emails INTEGER,\n"
                 ++ "right_report_issues INTEGER,\n"
                 ++ "right_modify_issues INTEGER,\n"
                 ++ "right_upload_files INTEGER,\n"
                 ++ "right_comment_issues INTEGER\n"
                 ++ ")"
  userCount <- eval database "SELECT count(*) FROM users"
  if userCount == SQL.SQLInteger 0
     then do
       run' database ("INSERT INTO users (id, full_name, email, password_hash, "
                      ++ "right_admin_users, right_see_emails, right_report_issues, "
                      ++ "right_modify_issues, right_upload_files, right_comment_issues) "
                      ++ "VALUES (1, 'Dan Knapp', 'dankna@gmail.com', ?, "
                      ++ "1, 1, 1, 1, 1, 1)")
                     [SQL.SQLBlob $ hashPassword "This password must be changed."]
       run database ("INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_admin_users, right_see_emails, right_report_issues, "
                     ++ "right_modify_issues, right_upload_files, right_comment_issues) "
                     ++ "VALUES (2, 'Nobody', 'nobody', NULL, 0, 0, 0, 0, 0, 0)")
       run database ("INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_admin_users, right_see_emails, right_report_issues, "
                     ++ "right_modify_issues, right_upload_files) "
                     ++ "VALUES (3, 'Anonymous', 'anonymous', NULL, 0, 0, 1, 0, 0, 1)")
     else return ()
  run database $ "CREATE TABLE IF NOT EXISTS user_issue_changes (\n"
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
                 ++ ")"
  run database $ "CREATE TABLE IF NOT EXISTS user_issue_comments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "text TEXT,\n"
                 ++ "CONSTRAINT key PRIMARY KEY (user, issue, timestamp)\n"
                 ++ ")"
  run database $ "CREATE TABLE IF NOT EXISTS user_issue_attachments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "filename TEXT,\n"
                 ++ "data BLOB,\n"
                 ++ "CONSTRAINT key1 PRIMARY KEY (user, issue, timestamp),\n"
                 ++ "CONSTRAINT key2 UNIQUE (issue, filename)\n"
                 ++ ")"


run :: SQL.Database -> String -> IO ()
run database query = do
  statement <- SQL.prepare database query
  SQL.step statement
  SQL.finalize statement


run' :: SQL.Database -> String -> [SQL.SQLData] -> IO ()
run' database query bindings = do
  statement <- SQL.prepare database query
  SQL.bind statement bindings
  SQL.step statement
  SQL.finalize statement


eval :: SQL.Database -> String -> IO SQL.SQLData
eval database query = do
  statement <- SQL.prepare database query
  SQL.step statement
  result <- SQL.column statement 0
  SQL.finalize statement
  return result

module Main (main) where

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

import qualified Network.FruitTart.Controller.Captcha
import qualified Network.FruitTart.Controller.Login
import qualified Network.FruitTart.Dispatcher as Dispatcher
import qualified Database.SQLite3 as SQL
import Network.FruitTart.Util


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- SQL.open databasePath
  initDatabase database
  captchaCacheMVar <- newMVar $ Map.empty
  state <- return $ FruitTartState {
             database = database,
             sessionID = Nothing,
             captchaCacheMVar = captchaCacheMVar
           }
  runFastCGIorCGI $ evalStateT (Dispatcher.processRequest dispatchTable) state
  return ()


dispatchTable :: ControllerTable
dispatchTable
    = Map.fromList
      [("login", Network.FruitTart.Controller.Login.actionTable),
       ("captcha", Network.FruitTart.Controller.Captcha.actionTable)]


schemaVersion :: Int64
schemaVersion = 1


requireModuleVersion :: Database -> String -> Int64 -> IO ()
requireModuleVersion database module requiredVersion = do
  [[SQLInteger count]]
      <- earlyQuery database
                    "SELECT count(*) FROM schema_versions WHERE module = ?"
                    [SQLText module]
  case count of
    0 -> do
      earlyQuery database
                 "INSERT INTO schema_versions (module, version) VALUES (?, ?)"
                 [SQLText module, SQLInteger requiredVersion]
    1 -> do
      [[SQLInteger presentVersion]]
          <- earlyQuery database
                        "SELECT version FROM schema_versions WHERE module = ?"
                        [SQLText module]
      if presentVersion /= requiredVersion
        then do
          logCGI $ "Schema mismatch for module " ++ module ++ ": Program version "
                 ++ (show requiredVersion) ++ ", database version "
                 ++ (show presentVersion) ++ "."
          exitFailure
        else return ()


initDatabase :: SQL.Database -> IO ()
initDatabase database = do
  requireModuleVersion "FruitTart" schemaVersion
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "timestamp_activity INTEGER,\n"
                 ++ "recent_user INTEGER,\n"
                 ++ "logged_in_user INTEGER,\n"
                 ++ "popup_message TEXT\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS settings (\n"
                 ++ "anonymous_user INTEGER"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE IF NOT EXISTS users (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "full_name TEXT,\n"
                 ++ "email TEXT,\n"
                 ++ "password_hash BLOB,\n"
                 ++ "right_synchronize INTEGER,\n"
                 ++ "right_admin_users INTEGER,\n"
                 ++ "right_see_emails INTEGER,\n"
                 ++ "right_report_issues INTEGER,\n"
                 ++ "right_modify_issues INTEGER,\n"
                 ++ "right_upload_files INTEGER,\n"
                 ++ "right_comment_issues INTEGER\n"
                 ++ ")")
                 []
  [[userCount]] <- earlyQuery database "SELECT count(*) FROM users" []
  if userCount == SQLInteger 0
     then do
       earlyQuery database
                      (  "INSERT INTO users (id, full_name, email, password_hash, "
                      ++ "right_synchronize, right_admin_users, right_see_emails, "
                      ++ "right_report_issues, right_modify_issues, right_upload_files, "
                      ++ "right_comment_issues) "
                      ++ "VALUES (1, 'Dan Knapp', 'dankna@gmail.com', ?, "
                      ++ "1, 1, 1, 1, 1, 1, 1)")
                      [SQLBlob $ hashPassword "This password must be changed."]
       earlyQuery database
                     (  "INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_synchronize, right_admin_users, right_see_emails, "
                     ++ "right_report_issues, right_modify_issues, right_upload_files, "
                     ++ "right_comment_issues) "
                     ++ "VALUES (2, 'Nobody', 'nobody', NULL, 0, 0, 0, 0, 0, 0, 0)")
                     []
       earlyQuery database
                     (  "INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_synchronize, right_admin_users, right_see_emails, "
                     ++ "right_report_issues, right_modify_issues, right_upload_files, "
                     ++ "right_comment_issues) "
                     ++ "VALUES (3, 'Anonymous', 'anonymous', NULL, 0, 0, 0, 1, 0, 0, 1)")
                     []
       [[SQLInteger count]] <- earlyQuery database "SELECT count(*) FROM settings"
       case count of
         0 -> earlyRun database "INSERT INTO settings (anonymous_user) VALUES (3)"
         _ -> earlyRun database "UPDATE settings SET anonymous_user = 3"
     else return ()

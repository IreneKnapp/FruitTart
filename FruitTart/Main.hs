module Main (main) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Network.FastCGI (runFastCGIorCGI)
import Network.CGI.Monad
import System.Environment
import System.Exit

import qualified Network.FruitTart.Controller.Captcha
import qualified Network.FruitTart.Controller.Login
import qualified Network.FruitTart.Dispatcher as Dispatcher
import Database.SQLite3
import Network.FruitTart.Util


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  requireModuleVersion database "FruitTart" schemaVersion
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
requireModuleVersion database name requiredVersion = do
  [[SQLInteger count]]
      <- earlyQuery database
                    "SELECT count(*) FROM schema_versions WHERE module = ?"
                    [SQLText name]
  case count of
    0 -> do
      initDatabase database
      return ()
    1 -> do
      [[SQLInteger presentVersion]]
          <- earlyQuery database
                        "SELECT version FROM schema_versions WHERE module = ?"
                        [SQLText name]
      if presentVersion /= requiredVersion
        then do
          logCGI $ "Schema mismatch for module " ++ name ++ ": Program version "
                 ++ (show requiredVersion) ++ ", database version "
                 ++ (show presentVersion) ++ "."
          exitFailure
        else return ()


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
                 (  "CREATE TABLE schema_versions (\n"
                 ++ "module TEXT PRIMARY KEY,\n"
                 ++ "version INTEGER"
                 ++ ")")
                 []
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES ('FruitTart', ?)"
             [SQLInteger schemaVersion]
  earlyQuery database
                 (  "CREATE TABLE sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "timestamp_activity INTEGER,\n"
                 ++ "recent_user INTEGER,\n"
                 ++ "logged_in_user INTEGER,\n"
                 ++ "popup_message TEXT\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE settings (\n"
                 ++ "owner_user INTEGER,"
                 ++ "anonymous_user INTEGER,"
                 ++ "nobody_user INTEGER,"
                 ++ "default_page TEXT"
                 ++ ")")
                 []
  earlyQuery database
                 (  "INSERT INTO settings "
                 ++ "(owner_user, anonymous_user, nobody_user, default_page) "
                 ++ "VALUES (NULL, NULL, NULL, '/login/account/')")
                 []
  earlyQuery database
                 (  "CREATE TABLE users (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "full_name TEXT,\n"
                 ++ "email TEXT,\n"
                 ++ "password_hash BLOB\n"
                 ++ ")")
                 []
  earlyQuery database
             (  "INSERT INTO users (id, full_name, email, password_hash) "
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
  earlyQuery database
            (  "UPDATE settings "
            ++ "SET owner_users = 1, anonymous_user = 3, nobody_user = 2")
            []
  return ()

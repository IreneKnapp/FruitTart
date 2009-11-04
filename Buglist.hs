module Buglist (main,
                getUser,
                getLoggedInUser,
                getEffectiveUser,
                getCanActAsUser,
                getRightSynchronize,
                getRightAdminUsers,
                getRightSeeEmails,
                getRightReportIssues,
                getRightModifyIssues,
                getRightUploadFiles,
                getRightCommentIssues,
                getPageHeadItems,
                getNavigationBar,
                getLoginButton,
                setPopupMessage,
                getPopupMessage,
                getSubnavigationBar,
                getStatusPopup,
                getResolutionPopup,
                getModulePopup,
                getSeverityPopup,
                getPriorityPopup,
                privacyNote,
                defaultFullName,
                defaultEmail
               )
    where

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
import qualified Network.FruitTart.Controller.Issues
import qualified Network.FruitTart.Controller.Login
import qualified Network.FruitTart.Controller.Users
import qualified Network.FruitTart.Controller.Synchronization
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
       ("issues", Network.FruitTart.Controller.Issues.actionTable),
       ("users", Network.FruitTart.Controller.Users.actionTable),
       ("captcha", Network.FruitTart.Controller.Captcha.actionTable),
       ("synchronization", Network.FruitTart.Controller.Synchronization.actionTable)]


schemaVersion :: Int64
schemaVersion = 1


initDatabase :: SQL.Database -> IO ()
initDatabase database = do
  earlyRun database $ "CREATE TABLE IF NOT EXISTS schema_version (\n"
                 ++ "version INTEGER\n"
                 ++ ")"
  schemaVersionCount <- earlyEval database "SELECT count(*) FROM schema_version"
  if schemaVersionCount == SQLInteger 0
     then do
       earlyRun' database
            "INSERT INTO schema_version (version) VALUES (?)"
            [SQLInteger schemaVersion]
       initDatabase' database
     else do
       SQLInteger databaseSchemaVersion
           <- earlyEval database "SELECT version FROM schema_version LIMIT 1"
       if databaseSchemaVersion /= schemaVersion
          then do
            logCGI $ "Schema mismatch: Program version " ++ (show schemaVersion)
                     ++ ", database version " ++ (show databaseSchemaVersion)
                     ++ "."
            exitFailure
          else do initDatabase' database


initDatabase' :: SQL.Database -> IO ()
initDatabase' database = do
  earlyRun database $ "CREATE TABLE IF NOT EXISTS sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "timestamp_activity INTEGER,\n"
                 ++ "recent_user INTEGER,\n"
                 ++ "logged_in_user INTEGER,\n"
                 ++ "popup_message TEXT,\n"
                 ++ "issue_index_filter_which TEXT,\n"
                 ++ "issue_index_filter_all_modules INTEGER,\n"
                 ++ "issue_index_filter_module INTEGER\n"
                 ++ ")"
  earlyRun database $ "CREATE TABLE IF NOT EXISTS settings (\n"
                 ++ "anonymous_user INTEGER"
                 ++ ")"
  earlyRun database $ "CREATE TABLE IF NOT EXISTS issues (\n"
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
                 ++ ")"
  earlyRun database $ "CREATE TABLE IF NOT EXISTS statuses (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  statusCount <- earlyEval database "SELECT count(*) FROM statuses"
  if statusCount == SQLInteger 0
     then do
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (1, 'NEW')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (2, 'REOPENED')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (3, 'UNCONFIRMED')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (4, 'CONFIRMED')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (5, 'ASSIGNED')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (6, 'RESOLVED')"
       earlyRun database $ "INSERT INTO statuses (id, name) VALUES (7, 'CLOSED')"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS resolutions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  resolutionCount <- earlyEval database "SELECT count(*) FROM resolutions"
  if resolutionCount == SQLInteger 0
     then do
       earlyRun database $ "INSERT INTO resolutions (id, name) VALUES (1, '---')"
       earlyRun database $ "INSERT INTO resolutions (id, name) VALUES (2, 'FIXED')"
       earlyRun database $ "INSERT INTO resolutions (id, name) VALUES (3, 'WONTFIX')"
       earlyRun database $ "INSERT INTO resolutions (id, name) VALUES (4, 'WORKSFORME')"
       earlyRun database $ "INSERT INTO resolutions (id, name) VALUES (5, 'DUPLICATE')"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS modules (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  moduleCount <- earlyEval database "SELECT count(*) FROM modules"
  if moduleCount == SQLInteger 0
     then do
       earlyRun database $ "INSERT INTO modules (id, name) VALUES (1, 'Program')"
       earlyRun database $ "INSERT INTO modules (id, name) VALUES (2, 'Documentation')"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS severities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  severityCount <- earlyEval database "SELECT count(*) FROM severities"
  if severityCount == SQLInteger 0
     then do
       earlyRun database $ "INSERT INTO severities (id, name) VALUES (1, 'critical')"
       earlyRun database $ "INSERT INTO severities (id, name) VALUES (2, 'major')"
       earlyRun database $ "INSERT INTO severities (id, name) VALUES (3, 'normal')"
       earlyRun database $ "INSERT INTO severities (id, name) VALUES (4, 'minor')"
       earlyRun database $ "INSERT INTO severities (id, name) VALUES (5, 'enhancement')"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS priorities (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT\n"
                 ++ ")"
  priorityCount <- earlyEval database "SELECT count(*) FROM priorities"
  if priorityCount == SQLInteger 0
     then do
       earlyRun database $ "INSERT INTO priorities (id, name) VALUES (1, 'high')"
       earlyRun database $ "INSERT INTO priorities (id, name) VALUES (2, 'normal')"
       earlyRun database $ "INSERT INTO priorities (id, name) VALUES (3, 'low')"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS issue_defaults (\n"
                 ++ "status INTEGER,\n"
                 ++ "resolution INTEGER,\n"
                 ++ "severity INTEGER,\n"
                 ++ "priority INTEGER\n"
                 ++ ")"
  defaultsCount <- earlyEval database "SELECT count(*) FROM issue_defaults"
  if defaultsCount == SQLInteger 0
     then do
       earlyRun database ("INSERT INTO issue_defaults "
                     ++ "(status, resolution, severity, priority) "
                     ++ "VALUES (1, 1, 3, 2)")
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS users (\n"
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
                 ++ ")"
  userCount <- earlyEval database "SELECT count(*) FROM users"
  if userCount == SQLInteger 0
     then do
       earlyRun' database ("INSERT INTO users (id, full_name, email, password_hash, "
                      ++ "right_synchronize, right_admin_users, right_see_emails, "
                      ++ "right_report_issues, right_modify_issues, right_upload_files, "
                      ++ "right_comment_issues) "
                      ++ "VALUES (1, 'Dan Knapp', 'dankna@gmail.com', ?, "
                      ++ "1, 1, 1, 1, 1, 1, 1)")
                     [SQLBlob $ hashPassword "This password must be changed."]
       earlyRun database ("INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_synchronize, right_admin_users, right_see_emails, "
                     ++ "right_report_issues, right_modify_issues, right_upload_files, "
                     ++ "right_comment_issues) "
                     ++ "VALUES (2, 'Nobody', 'nobody', NULL, 0, 0, 0, 0, 0, 0, 0)")
       earlyRun database ("INSERT INTO users (id, full_name, email, password_hash, "
                     ++ "right_synchronize, right_admin_users, right_see_emails, "
                     ++ "right_report_issues, right_modify_issues, right_upload_files, "
                     ++ "right_comment_issues) "
                     ++ "VALUES (3, 'Anonymous', 'anonymous', NULL, 0, 0, 0, 1, 0, 0, 1)")
       SQLInteger count <- earlyEval database "SELECT count(*) FROM settings"
       case count of
         0 -> earlyRun database "INSERT INTO settings (anonymous_user) VALUES (3)"
         _ -> earlyRun database "UPDATE settings SET anonymous_user = 3"
     else return ()
  earlyRun database $ "CREATE TABLE IF NOT EXISTS user_issue_changes (\n"
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
  earlyRun database $ "CREATE TABLE IF NOT EXISTS user_issue_comments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "text TEXT,\n"
                 ++ "CONSTRAINT key PRIMARY KEY (user, issue, timestamp)\n"
                 ++ ")"
  earlyRun database $ "CREATE TABLE IF NOT EXISTS user_issue_attachments (\n"
                 ++ "user INTEGER,\n"
                 ++ "issue INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "filename TEXT,\n"
                 ++ "data BLOB,\n"
                 ++ "CONSTRAINT key1 PRIMARY KEY (user, issue, timestamp),\n"
                 ++ "CONSTRAINT key2 UNIQUE (issue, filename)\n"
                 ++ ")"
  earlyRun database $ "CREATE TABLE IF NOT EXISTS navigation_items (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "name TEXT,\n"
                 ++ "link TEXT,\n"
                 ++ "within_buglist_tree INTEGER,\n"
                 ++ "separator INTEGER,\n"
                 ++ "always_enabled INTEGER,\n"
                 ++ "class TEXT\n"
                 ++ ")"
  navigationItemCount <- earlyEval database "SELECT count(*) FROM navigation_items"
  if navigationItemCount == SQLInteger 0
     then do
       earlyRun database ("INSERT INTO navigation_items "
                     ++ "(id, name, link, within_buglist_tree, separator, "
                     ++ "always_enabled, class) "
                     ++ "VALUES (1, 'Report an Issue', '/issues/create/', 1, 0, 0, NULL)")
       earlyRun database ("INSERT INTO navigation_items "
                     ++ "(id, name, link, within_buglist_tree, separator, "
                     ++ "always_enabled, class) "
                     ++ "VALUES (2, 'Issue List', '/issues/index/', 1, 0, 0, NULL)")
       earlyRun database ("INSERT INTO navigation_items "
                     ++ "(id, name, link, within_buglist_tree, separator, "
                     ++ "always_enabled, class) "
                     ++ "VALUES (3, 'User List', '/users/index/', 1, 0, 0, NULL)")
     else return ()


getUser :: String -> String -> FruitTart Int64
getUser fullName email = do
  if (email == "") || (email == "anonymous")
     then do
       rows <- query "SELECT id FROM users WHERE full_name == ? AND email == 'anonymous'"
                     [SQLText fullName]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, password_hash, "
                   ++ "right_admin_users, right_see_emails, right_report_issues, "
                   ++ "right_modify_issues, right_upload_files, right_comment_issues) "
                   ++ "VALUES (?, 'anonymous', NULL, 0, 0, 1, 0, 0, 1)")
                  [SQLText fullName]
            [[SQLInteger id]]
                <- query ("SELECT id FROM users WHERE full_name == ? "
                          ++ "AND email == 'anonymous'")
                         [SQLText fullName]
            return id
     else do
       rows <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, password_hash, "
                   ++ "right_admin_users, right_see_emails, right_report_issues, "
                   ++ "right_modify_issues, right_upload_files, right_comment_issues) "
                   ++ "VALUES (?, ?, NULL, 0, 0, 1, 0, 0, 1)")
                  [SQLText fullName, SQLText email]
            [[SQLInteger id]]
                <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
            return id


getLoggedInUser :: FruitTart (Maybe Int64)
getLoggedInUser = do
  sessionID <- Dispatcher.getSessionID
  [[maybeUserID]] <- query "SELECT logged_in_user FROM sessions WHERE id = ?"
                           [SQLInteger sessionID]
  return $ case maybeUserID of
             SQLNull -> Nothing
             SQLInteger userID -> Just userID


getEffectiveUser :: FruitTart Int64
getEffectiveUser = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Just userID -> return userID
    Nothing -> do
      [[SQLInteger anonymousID]] <- query "SELECT anonymous_user FROM settings LIMIT 1"
                                          []
      return anonymousID


getCanActAsUser :: Int64 -> FruitTart Bool
getCanActAsUser userID = do
  effectiveUserID <- getEffectiveUser
  if effectiveUserID == userID
     then return True
     else do
       [[SQLInteger isNull]]
           <- query "SELECT password_hash IS NULL FROM users WHERE id = ?"
                    [SQLInteger userID]
       return $ case isNull of
                  0 -> False
                  _ -> True


getRightSynchronize :: FruitTart Bool
getRightSynchronize = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_synchronize FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightAdminUsers :: FruitTart Bool
getRightAdminUsers = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_admin_users FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightSeeEmails :: FruitTart Bool
getRightSeeEmails = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_see_emails FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightReportIssues :: FruitTart Bool
getRightReportIssues = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_report_issues FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightModifyIssues :: FruitTart Bool
getRightModifyIssues = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_modify_issues FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightUploadFiles :: FruitTart Bool
getRightUploadFiles = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_upload_files FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightCommentIssues :: FruitTart Bool
getRightCommentIssues = do
  userID <- getEffectiveUser
  [[SQLInteger right]] <- query "SELECT right_comment_issues FROM users WHERE id = ?"
                                [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getPageHeadItems :: FruitTart String
getPageHeadItems
    = return 
      ("<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<link href=\"/css/navigation.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<script src=\"/js/jquery.js\" type=\"text/ecmascript\"></script>\n"
       ++ "<script src=\"/js/buglist.js\" type=\"text/ecmascript\"></script>\n")


getNavigationBar :: String -> FruitTart String
getNavigationBar currentPage = do
  items <- query ("SELECT name, link, separator, always_enabled, class "
                  ++ "FROM navigation_items ORDER BY id")
                 []
  let item name link separator alwaysEnabled maybeClass =
          let class' = case maybeClass of
                         Nothing -> ""
                         Just class' -> " class=\"" ++ class' ++ "\""
          in if separator
             then "<div " ++ class' ++ "></div>"
             else if (link /= currentPage) || alwaysEnabled
                  then "<a href=\"" ++ (escapeAttribute link) ++ "\"" ++ class' ++ ">"
                       ++ (escapeHTML name) ++ "</a>"
                  else "<b" ++ class' ++ ">" ++ (escapeHTML name) ++ "</b>"
  result <- return $ "<div id=\"navigation\">"
           ++ (concat $ map (\[SQLText name,
                               SQLText link,
                               SQLInteger separator,
                               SQLInteger alwaysEnabled,
                               maybeClass]
                             -> item name
                                     link
                                     (separator /= 0)
                                     (alwaysEnabled /= 0)
                                     (case maybeClass of
                                        SQLNull -> Nothing
                                        SQLText class' -> Just class'))
                             items)
           ++ "</div>\n"
  return result


getLoginButton :: String -> FruitTart String
getLoginButton currentPage = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Nothing -> do
      sessionID <- Dispatcher.getSessionID
      [[maybeEmail]] <- query ("SELECT users.email FROM sessions LEFT JOIN users "
                               ++ "ON users.id = sessions.recent_user "
                               ++ "WHERE sessions.id = ?")
                              [SQLInteger sessionID]
      email <- return $ case maybeEmail of
                          SQLNull -> ""
                          SQLText email -> email
      return ("<div id=\"navigationright\">"
              ++ "<a id=\"loginlink\" class=\"login\" href=\"/login/login/\">Log In</a>"
              ++ "</div>\n"
              ++ "<div id=\"loginbox\" style=\"display: none;\">"
              ++ "<form method=\"POST\" action=\"/login/login/\">\n"
              ++ "<div><b>Email:</b> "
              ++ "<input type=\"text\" size=\"15\" name=\"email\" value=\""
              ++ (escapeAttribute email)
              ++ "\"/></div>\n"
              ++ "<div><b>Password:</b> "
              ++ "<input type=\"password\" size=\"10\" name=\"password\" value=\""
              ++ "\"/></div>\n"
              ++ "<div class=\"submit\">"
              ++ "<button type=\"submit\" value=\"Log In\">Log In</button>"
              ++ "</div>\n"
              ++ "</form>\n"
              ++ "</div>\n")
    Just _ -> do
      return ("<div id=\"navigationright\">"
              ++ (if currentPage == "/login/account/"
                     then "<b>Account</b>"
                     else "<a href=\"/login/account/\">Account</a>")
              ++ "<a id=\"loginlink\" class=\"logout\" href=\"/login/logout/\">"
              ++ "Log Out</a></div>\n")


setPopupMessage :: Maybe String -> FruitTart ()
setPopupMessage maybeMessage = do
  sessionID <- Dispatcher.getSessionID
  query "UPDATE sessions SET popup_message = ? WHERE id = ?"
        [case maybeMessage of
           Nothing -> SQLNull
           Just message -> SQLText message,
         SQLInteger sessionID]
  return ()


getPopupMessage :: FruitTart String
getPopupMessage = do
  sessionID <- Dispatcher.getSessionID
  [[maybeMessage]] <- query "SELECT popup_message FROM sessions WHERE id = ?"
                            [SQLInteger sessionID]
  query "UPDATE sessions SET popup_message = NULL WHERE ID = ?"
        [SQLInteger sessionID]
  case maybeMessage of
    SQLNull -> return ""
    SQLText message -> return $ "<div id=\"popupmessage\">"
                              ++ (escapeHTML message)
                              ++ "</div>\n"


getSubnavigationBar :: String -> [Maybe (String, String)] -> FruitTart String
getSubnavigationBar currentPage items = do
  let item name link =
          if (link /= currentPage)
             then "<a href=\"" ++ (escapeAttribute link) ++ "\">"
                  ++ (escapeHTML name) ++ "</a>"
             else "<b>" ++ (escapeHTML name) ++ "</b>"
  return $ "<div class=\"navigation\">"
         ++ (concat $ map (\maybeItem -> case maybeItem of
                                           Just (name, link) -> item name link
                                           Nothing -> "<div class=\"separator\"></div>")
                          items)
         ++ "</div>\n"


getStatusPopup :: Maybe Int64 -> FruitTart String
getStatusPopup maybeStatusID = do
  statuses <- query "SELECT id, name FROM statuses ORDER BY id" []
  return $ "<select name=\"status\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if Just id == maybeStatusID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          statuses)
         ++ "</select>\n"


getResolutionPopup :: Maybe Int64 -> FruitTart String
getResolutionPopup maybeResolutionID = do
  resolutions <- query "SELECT id, name FROM resolutions ORDER BY id" []
  return $ "<select name=\"resolution\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if Just id == maybeResolutionID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          resolutions)
         ++ "</select>\n"


getModulePopup :: Maybe Int64 -> FruitTart String
getModulePopup maybeModuleID = do
  modules <- query "SELECT id, name FROM modules ORDER BY id" []
  return $ "<select name=\"module\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if Just id == maybeModuleID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          modules)
         ++ "</select>\n"


getSeverityPopup :: Maybe Int64 -> FruitTart String
getSeverityPopup maybeSeverityID = do
  severities <- query "SELECT id, name FROM severities ORDER BY id" []
  return $ "<select name=\"severity\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if Just id == maybeSeverityID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          severities)
         ++ "</select>\n"


getPriorityPopup :: Maybe Int64 -> FruitTart String
getPriorityPopup maybePriorityID = do
  priorities <- query "SELECT id, name FROM priorities ORDER BY id" []
  return $ "<select name=\"priority\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if Just id == maybePriorityID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          priorities)
         ++ "</select>\n"


privacyNote :: String
privacyNote
    = "Your email will be visible only to members who have accounts."


defaultFullName :: String
defaultFullName = "Anonymous"


defaultEmail :: String
defaultEmail = "anonymous"

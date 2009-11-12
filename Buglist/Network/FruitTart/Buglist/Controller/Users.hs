module Network.FruitTart.Buglist.Controller.Users (
                                                   actionTable,
                                                   functionTable,
                                                   getRightSynchronize,
                                                   getRightAdminUsers,
                                                   getRightSeeEmails,
                                                   getRightReportIssues,
                                                   getRightModifyIssues,
                                                   getRightUploadFiles,
                                                   getRightCommentIssues
                                                  )
    where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import Network.FruitTart.Base
import Network.FruitTart.Buglist.Imports
import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view)]


functionTable :: FunctionTable
functionTable
    = makeFunctionTable [("getRightSynchronize", toDyn getRightSynchronize),
                         ("getRightAdminUsers", toDyn getRightAdminUsers),
                         ("getRightSeeEmails", toDyn getRightSeeEmails),
                         ("getRightReportIssues", toDyn getRightReportIssues),
                         ("getRightModifyIssues", toDyn getRightModifyIssues),
                         ("getRightUploadFiles", toDyn getRightUploadFiles),
                         ("getRightCommentIssues", toDyn getRightCommentIssues)]


index :: FruitTart CGIResult
index = do
  bind "Templates" "pageTitle" "Buglist Users"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  currentPage <- return "/users/index/"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bindQueryMultipleRows "Buglist.Controller.Users" "rows"
                        [("id", TInt),
                         ("fullName", TString),
                         ("email", TString)]
                        "SELECT id, full_name, email FROM users" []
  pageContent <- getTemplate "Buglist.Controller.Users" "index"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


view :: Int64 -> FruitTart CGIResult
view id = do
  rows <- query "SELECT full_name, email FROM users WHERE id = ?"
                [SQLInteger id]
  case rows of
    [[SQLText fullName, SQLText email]]
      -> do
       creations <- query (  "SELECT "
                          ++ "issues.timestamp_created, "
                          ++ "'Create', "
                          ++ "issues.summary, "
                          ++ "issues.id "
                          ++ "FROM buglist_issues AS issues "
                          ++ "WHERE issues.reporter = ? "
                          ++ "ORDER BY issues.timestamp_created DESC")
                          [SQLInteger id]
                    >>= return . map
                                 (convertRowToBindings "Buglist.Controller.Users"
                                                       [("timestamp", TInt),
                                                        ("action", TString),
                                                        ("summary", TString),
                                                        ("id", TInt)])
       changes <- query (  "SELECT "
                        ++ "user_issue_changes.timestamp, "
                        ++ "'Edit', "
                        ++ "issues.summary, "
                        ++ "issues.id "
                        ++ "FROM buglist_user_issue_changes AS user_issue_changes "
                        ++ "INNER JOIN users "
                        ++ "ON user_issue_changes.user = users.id "
                        ++ "INNER JOIN buglist_issues AS issues "
                        ++ "ON user_issue_changes.issue = issues.id "
                        ++ "WHERE users.id = ? "
                        ++ "ORDER BY user_issue_changes.timestamp DESC")
                        [SQLInteger id]
                  >>= return . map
                               (convertRowToBindings "Buglist.Controller.Users"
                                                     [("timestamp", TInt),
                                                      ("action", TString),
                                                      ("summary", TString),
                                                      ("id", TInt)])
       comments <- query (  "SELECT "
                         ++ "user_issue_comments.timestamp, "
                         ++ "'Comment', "
                         ++ "issues.summary, "
                         ++ "issues.id "
                         ++ "FROM buglist_user_issue_comments AS user_issue_comments "
                         ++ "INNER JOIN users "
                         ++ "ON user_issue_comments.user = users.id "
                         ++ "INNER JOIN buglist_issues AS issues "
                         ++ "ON user_issue_comments.issue = issues.id "
                         ++ "WHERE users.id = ? "
                         ++ "ORDER BY user_issue_comments.timestamp DESC")
                         [SQLInteger id]
                   >>= return . map
                                (convertRowToBindings "Buglist.Controller.Users"
                                                      [("timestamp", TInt),
                                                       ("action", TString),
                                                       ("summary", TString),
                                                       ("id", TInt)])
       files <- query (  "SELECT "
                      ++ "user_issue_attachments.timestamp, "
                      ++ "'Attachment', "
                      ++ "issues.summary, "
                      ++ "issues.id "
                      ++ "FROM buglist_user_issue_attachments AS user_issue_attachments "
                      ++ "INNER JOIN users "
                      ++ "ON user_issue_attachments.user = users.id "
                      ++ "INNER JOIN buglist_issues AS issues ON "
                      ++ "user_issue_attachments.issue = issues.id "
                      ++ "WHERE users.id = ? "
                      ++ "ORDER BY user_issue_attachments.timestamp DESC")
                      [SQLInteger id]
                >>= return . map
                             (convertRowToBindings "Buglist.Controller.Users"
                                                   [("timestamp", TInt),
                                                    ("action", TString),
                                                    ("summary", TString),
                                                    ("id", TInt)])
       rows <- return $ mergeBy (\bindingsA bindingsB ->
                                 let key = ("Buglist.Controller.Users", "timestamp")
                                     TemplateInteger a
                                         = fromJust $ Map.lookup key bindingsA
                                     TemplateInteger b
                                         = fromJust $ Map.lookup key bindingsB
                                 in case compare a b of
                                      LT -> GT
                                      GT -> LT
                                      EQ -> EQ)
                                [changes, files, comments, creations]
       bind "Templates" "pageTitle" $ escapeHTML fullName
       pageHeadItems <- getPageHeadItems
       bind "Templates" "pageHeadItems" pageHeadItems
       currentPage <- return $ "/users/view/" ++ (show id) ++ "/"
       navigationBar <- getNavigationBar currentPage
       bind "Templates" "navigationBar" navigationBar
       loginButton <- getLoginButton currentPage
       bind "Templates" "loginButton" loginButton
       popupMessage <- getPopupMessage
       bind "Templates" "popupMessage" popupMessage
       bind "Buglist.Controller.Users" "fullName" fullName
       bind "Buglist.Controller.Users" "email" email
       bind "Buglist.Controller.Users" "rows" rows
       pageContent <- getTemplate "Buglist.Controller.Users" "view"
       bind "Templates" "pageContent" pageContent
       page <- getTemplate "Templates" "page"
       output page
    _ -> errorInvalidID "user"


getRightSynchronize :: FruitTart Bool
getRightSynchronize = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_synchronize FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightAdminUsers :: FruitTart Bool
getRightAdminUsers = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_admin_users FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightSeeEmails :: FruitTart Bool
getRightSeeEmails = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_see_emails FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightReportIssues :: FruitTart Bool
getRightReportIssues = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_report_issues FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightModifyIssues :: FruitTart Bool
getRightModifyIssues = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_modify_issues FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightUploadFiles :: FruitTart Bool
getRightUploadFiles = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_upload_files FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


getRightCommentIssues :: FruitTart Bool
getRightCommentIssues = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_comment_issues FROM buglist_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True

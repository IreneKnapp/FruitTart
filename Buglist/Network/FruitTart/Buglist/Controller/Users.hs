module Network.FruitTart.Buglist.Controller.Users (actionTable) where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Buglist
import Network.FruitTart.Dispatcher
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = Map.fromList [("index",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn index))]),
                      ("view",
                       Map.fromList [("GET", ([IDParameter],
                                              [],
                                              toDyn view))])]


index :: FruitTart CGIResult
index = do
  rows <- query "SELECT id, full_name, email FROM users" []
  pageHeadItems <- getPageHeadItems
  currentPage <- return "/users/index/"
  navigationBar <- getNavigationBar currentPage
  loginButton <- getLoginButton currentPage
  popupMessage <- getPopupMessage
  output  $ "<html><head>\n"
         ++ "<title>Buglist Users</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1>Buglist Users</h1>\n"
         ++ "<table>\n"
         ++ "<tr><th>Full Name</th><th>Email</th></tr>\n"
         ++ (concat $ map (\[SQLInteger id, SQLText fullName, SQLText email] ->
                  "<tr><td><a href=\"/users/view/" ++ (show id) ++ "/\">"
                  ++ (escapeHTML fullName) ++ "</a></td>"
                  ++ "<td><a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
                  ++ (escapeHTML email) ++ "</a></td></tr>\n")
                 rows)
         ++ "</table>\n"
         ++ "</body></html>"


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
       rows <- return $ mergeBy (\[SQLInteger a, _, _, _] [SQLInteger b, _, _, _]
                                 -> case compare a b of
                                      LT -> GT
                                      GT -> LT
                                      EQ -> EQ)
                                [changes, files, comments, creations]
       pageHeadItems <- getPageHeadItems
       currentPage <- return $ "/users/view/" ++ (show id) ++ "/"
       navigationBar <- getNavigationBar currentPage
       loginButton <- getLoginButton currentPage
       popupMessage <- getPopupMessage
       output
         $  "<html><head>\n"
         ++ "<title>" ++ (escapeHTML fullName) ++ "</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1><a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
         ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a></h1>\n"
         ++ "<table>\n"
         ++ "<tr><th>When</th><th>Action</th><th>Issue</th></tr>\n"
         ++ (concat $ map (\[SQLInteger timestamp,
                             SQLText action,
                             SQLText summary,
                             SQLInteger id]
                           -> "<tr><td>" ++ (escapeHTML $ timestampToString timestamp)
                              ++ "</td><td>" ++ (escapeHTML action)
                              ++ "</td><td><a href=\"/issues/view/"
                              ++ (show id) ++ "/\">" ++ (escapeHTML summary)
                              ++ "</a></td></tr>")
                          rows)
         ++ "</table>\n"
         ++ "</body></html>"
    _ -> errorInvalidID "user"

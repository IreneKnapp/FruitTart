module Controller.Users where

import Data.List

import Database
import {-# SOURCE #-} Dispatcher
import HTML
import Lists
import SQLite3 (SQLData(..))

index :: Buglist CGIResult
index = do
  rows <- query "SELECT id, full_name, email FROM users" []
  output  $ "<html><head>\n"
         ++ "<title>Buglist Users</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
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


view :: Int -> Buglist CGIResult
view id = do
  rows <- query "SELECT full_name, email FROM users WHERE id = ?"
                [SQLInteger $ fromIntegral id]
  case rows of
    [[SQLText fullName, SQLText email]]
      -> do
       changes <- query ("SELECT "
                         ++ "user_issue_changes.timestamp, "
                         ++ "'Edit', "
                         ++ "issues.summary, "
                         ++ "issues.id "
                         ++ "FROM user_issue_changes INNER JOIN users "
                         ++ "ON user_issue_changes.user = users.id "
                         ++ "INNER JOIN issues "
                         ++ "ON user_issue_changes.issue = issues.id "
                         ++ "WHERE users.id = ? "
                         ++ "ORDER BY user_issue_changes.timestamp")
                        [SQLInteger $ fromIntegral id]
       comments <- query ("SELECT "
                         ++ "user_issue_comments.timestamp, "
                         ++ "'Comment', "
                         ++ "issues.summary, "
                         ++ "issues.id "
                         ++ "FROM user_issue_comments INNER JOIN users "
                         ++ "ON user_issue_comments.user = users.id "
                         ++ "INNER JOIN issues "
                         ++ "ON user_issue_comments.issue = issues.id "
                         ++ "WHERE users.id = ? "
                         ++ "ORDER BY user_issue_comments.timestamp")
                        [SQLInteger $ fromIntegral id]
       files <- query ("SELECT "
                         ++ "user_issue_attachments.timestamp, "
                         ++ "'Attachment', "
                         ++ "issues.summary, "
                         ++ "issues.id "
                         ++ "FROM user_issue_attachments INNER JOIN users "
                         ++ "ON user_issue_attachments.user = users.id "
                         ++ "INNER JOIN issues ON "
                         ++ "user_issue_attachments.issue = issues.id "
                         ++ "WHERE users.id = ? "
                         ++ "ORDER BY user_issue_attachments.timestamp")
                        [SQLInteger $ fromIntegral id]
       rows <- return $ mergeBy (\[SQLInteger a, _, _] [SQLInteger b, _, _]
                                 -> compare a b)
                                [changes, comments, files]
       output
         $  "<html><head>\n"
         ++ "<title>" ++ (escapeHTML fullName) ++ "</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
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

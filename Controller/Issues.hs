module Controller.Issues where

import Data.Int
import Data.List
import Network.FastCGI hiding (output)

import Database
import {-# SOURCE #-} Dispatcher
import HTML
import Lists
import SQLite3 (SQLData(..))
import Text

index :: Buglist CGIResult
index = do
  values <- query ("SELECT "
                   ++ "issues.id, "
                   ++ "statuses.name, "
                   ++ "resolutions.name, "
                   ++ "modules.name, "
                   ++ "severities.name, "
                   ++ "priorities.name, "
                   ++ "assignee.email, "
                   ++ "reporter.email, "
                   ++ "issues.summary, "
                   ++ "issues.timestamp_created, "
                   ++ "issues.timestamp_modified "
                   ++ "FROM issues INNER JOIN statuses ON issues.status = statuses.id "
                   ++ "INNER JOIN resolutions ON issues.resolution = resolutions.id "
                   ++ "INNER JOIN modules ON issues.module = modules.id "
                   ++ "INNER JOIN severities ON issues.severity = severities.id "
                   ++ "INNER JOIN priorities ON issues.priority = priorities.id "
                   ++ "INNER JOIN users AS assignee ON issues.assignee = assignee.id "
                   ++ "INNER JOIN users AS reporter ON issues.reporter = reporter.id "
                   ++ "ORDER BY issues.timestamp_modified")
                  []
  output  $ "<html><head>\n"
         ++ "<title>Buglist Issues</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ "<h1>Buglist Issues</h1>\n"
         ++ "<table>\n"
         ++ "<tr><th>ID</th><th>Created</th><th>Modified</th><th>Stat</th><th>Res</th>"
         ++ "<th>Mod</th><th>Sev</th><th>Pri</th>"
         ++ "<th>Summary</th></tr>\n"
         ++ (concat $ map (\[SQLInteger id,
                             SQLText status,
                             SQLText resolution,
                             SQLText module',
                             SQLText severity,
                             SQLText priority,
                             SQLText assignee,
                             SQLText reporter,
                             SQLText summary,
                             SQLInteger created,
                             SQLInteger modified] ->
                  "<tr><td><a href=\"/issues/view/" ++ (show id) ++ "/\">"
                  ++ (show id) ++ "</a></td>"
                  ++ "<td>" ++ (escapeHTML $ timestampToString created) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML $ timestampToString modified) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML status) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML resolution) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML module') ++ "</td>"
                  ++ "<td>" ++ (escapeHTML severity) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML priority) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML summary) ++ "</td>"
                  ++ "</tr>\n")
                 values)
         ++ "</table>\n"
         ++ "</body></html>"


view :: Int64 -> Buglist CGIResult
view id = do
  info <- query ("SELECT "
                 ++ "statuses.name, "
                 ++ "resolutions.name, "
                 ++ "modules.name, "
                 ++ "severities.name, "
                 ++ "priorities.name, "
                 ++ "assignee.full_name, "
                 ++ "assignee.email, "
                 ++ "reporter.full_name, "
                 ++ "reporter.email, "
                 ++ "issues.summary, "
                 ++ "issues.timestamp_created, "
                 ++ "issues.timestamp_modified "
                 ++ "FROM issues INNER JOIN statuses ON issues.status = statuses.id "
                 ++ "INNER JOIN resolutions ON issues.resolution = resolutions.id "
                 ++ "INNER JOIN modules ON issues.module = modules.id "
                 ++ "INNER JOIN severities ON issues.severity = severities.id "
                 ++ "INNER JOIN priorities ON issues.priority = priorities.id "
                 ++ "INNER JOIN users AS assignee ON issues.assignee = assignee.id "
                 ++ "INNER JOIN users AS reporter ON issues.reporter = reporter.id "
                 ++ "WHERE issues.id = ?")
                [SQLInteger $ fromIntegral id]
  case info of
    [[SQLText status,
      SQLText resolution,
      SQLText module',
      SQLText severity,
      SQLText priority,
      SQLText assigneeFullName,
      SQLText assigneeEmail,
      SQLText reporterFullName,
      SQLText reporterEmail,
      SQLText summary,
      SQLInteger timestampCreated,
      SQLInteger timestampModified]]
      -> do
       changes <- query ("SELECT "
                         ++ "user_issue_changes.timestamp, "
                         ++ "users.full_name, "
                         ++ "users.email, "
                         ++ "'Edit', "
                         ++ "user_issue_changes.status_changed, "
                         ++ "user_issue_changes.resolution_changed, "
                         ++ "user_issue_changes.module_changed, "
                         ++ "user_issue_changes.severity_changed, "
                         ++ "user_issue_changes.priority_changed, "
                         ++ "user_issue_changes.assignee_changed, "
                         ++ "user_issue_changes.summary_changed, "
                         ++ "old_status.name, "
                         ++ "old_resolution.name, "
                         ++ "old_module.name, "
                         ++ "old_severity.name, "
                         ++ "old_priority.name, "
                         ++ "old_assignee.full_name, "
                         ++ "old_assignee.email, "
                         ++ "user_issue_changes.old_summary, "
                         ++ "new_status.name, "
                         ++ "new_resolution.name, "
                         ++ "new_module.name, "
                         ++ "new_severity.name, "
                         ++ "new_priority.name, "
                         ++ "new_assignee.full_name, "
                         ++ "new_assignee.email, "
                         ++ "user_issue_changes.new_summary "
                         ++ "FROM user_issue_changes INNER JOIN users "
                         ++ "ON user_issue_changes.user = users.id "
                         ++ "INNER JOIN issues "
                         ++ "ON user_issue_changes.issue = issues.id "
                         ++ "LEFT JOIN statuses AS old_status "
                         ++ "ON user_issue_changes.old_status = old_status.id "
                         ++ "LEFT JOIN resolutions AS old_resolution "
                         ++ "ON user_issue_changes.old_resolution = old_resolution.id "
                         ++ "LEFT JOIN modules AS old_module "
                         ++ "ON user_issue_changes.old_module = old_module.id "
                         ++ "LEFT JOIN severities AS old_severity "
                         ++ "ON user_issue_changes.old_severity = old_severity.id "
                         ++ "LEFT JOIN priorities AS old_priority "
                         ++ "ON user_issue_changes.old_priority = old_priority.id "
                         ++ "LEFT JOIN users AS old_assignee "
                         ++ "ON user_issue_changes.old_assignee = old_assignee.id "
                         ++ "LEFT JOIN statuses AS new_status "
                         ++ "ON user_issue_changes.new_status = new_status.id "
                         ++ "LEFT JOIN resolutions AS new_resolution "
                         ++ "ON user_issue_changes.new_resolution = new_resolution.id "
                         ++ "LEFT JOIN modules AS new_module "
                         ++ "ON user_issue_changes.new_module = new_module.id "
                         ++ "LEFT JOIN severities AS new_severity "
                         ++ "ON user_issue_changes.new_severity = new_severity.id "
                         ++ "LEFT JOIN priorities AS new_priority "
                         ++ "ON user_issue_changes.new_priority = new_priority.id "
                         ++ "LEFT JOIN users AS new_assignee "
                         ++ "ON user_issue_changes.new_assignee = new_assignee.id "
                         ++ "WHERE issues.id = ? "
                         ++ "ORDER BY user_issue_changes.timestamp")
                        [SQLInteger $ fromIntegral id]
       comments <- query ("SELECT "
                          ++ "user_issue_comments.timestamp, "
                          ++ "users.full_name, "
                          ++ "users.email, "
                          ++ "'Comment', "
                          ++ "user_issue_comments.text "
                          ++ "FROM user_issue_comments INNER JOIN users "
                          ++ "ON user_issue_comments.user = users.id "
                          ++ "INNER JOIN issues "
                          ++ "ON user_issue_comments.issue = issues.id "
                          ++ "WHERE issues.id = ? "
                          ++ "ORDER BY user_issue_comments.timestamp")
                         [SQLInteger $ fromIntegral id]
       files <- query ("SELECT "
                       ++ "user_issue_attachments.timestamp, "
                       ++ "users.full_name, "
                       ++ "users.email, "
                       ++ "'Attachment', "
                       ++ "user_issue_attachments.filename, "
                       ++ "length(user_issue_attachments.data) AS size "
                       ++ "FROM user_issue_attachments INNER JOIN users "
                       ++ "ON user_issue_attachments.user = users.id "
                       ++ "INNER JOIN issues ON "
                       ++ "user_issue_attachments.issue = issues.id "
                       ++ "WHERE issues.id = ? "
                       ++ "ORDER BY user_issue_attachments.timestamp")
                      [SQLInteger $ fromIntegral id]
       rows <- return $ mergeBy (\(SQLInteger a:_) (SQLInteger b:_)
                                  -> compare a b)
                                [changes, comments, files]
       output
         $  "<html><head>\n"
         ++ "<title>" ++ (escapeHTML summary) ++ "</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ "<h1>" ++ (escapeHTML summary) ++ "</h1>\n"
         ++ "<table class=\"layout\">\n"
         ++ "<tr><td>"
         ++ "<b>Status:</b> " ++ (escapeHTML status) ++ "<br />\n"
         ++ "<b>Resolution:</b> " ++ (escapeHTML resolution) ++ "<br />\n"
         ++ "<b>Module:</b> " ++ (escapeHTML module') ++ "<br />\n"
         ++ "<b>Severity:</b> " ++ (escapeHTML severity) ++ "<br />\n"
         ++ "<b>Priority:</b> " ++ (escapeHTML priority) ++ "<br />\n"
         ++ "</td><td>"
         ++ "<b>Assignee:</b> <a href=\"mailto:" ++ (escapeAttribute assigneeEmail)
         ++ "\">" ++ (escapeHTML assigneeFullName) ++ " &lt;"
         ++ (escapeHTML assigneeEmail) ++ "&gt;</a><br />\n"
         ++ "<b>Reporter:</b> <a href=\"mailto:" ++ (escapeAttribute reporterEmail)
         ++ "\">" ++ (escapeHTML reporterFullName) ++ " &lt;"
         ++ (escapeHTML reporterEmail) ++ "&gt;</a><br />\n"
         ++ "<b>Created:</b> " ++ (escapeHTML $ timestampToString timestampCreated)
         ++ "<br />\n"
         ++ "<b>Modified:</b> " ++ (escapeHTML $ timestampToString timestampModified)
         ++ "<br />\n"
         ++ "</td></tr>\n"
         ++ "</table>\n"
         ++ (concat $ map (\row -> viewDetail id row) rows)
         ++ "</body></html>"
    _ -> errorInvalidID "issue"


viewDetail :: Int64 -> [SQLData] -> String
viewDetail id row@(_:_:_:SQLText type':_) =
    case type' of
      _ | type' == "Edit" -> viewEditDetail row
        | type' == "Comment" -> viewCommentDetail row
        | type' == "Attachment" -> viewAttachmentDetail id row
        | True -> ""


viewEditDetail :: [SQLData] -> String
viewEditDetail [SQLInteger timestamp,
                SQLText fullName,
                SQLText email,
                _,
                SQLInteger statusChanged,
                SQLInteger resolutionChanged,
                SQLInteger moduleChanged,
                SQLInteger severityChanged,
                SQLInteger priorityChanged,
                SQLInteger assigneeChanged,
                SQLInteger summaryChanged,
                SQLText oldStatus,
                SQLText oldResolution,
                SQLText oldModule,
                SQLText oldSeverity,
                SQLText oldPriority,
                SQLText oldAssigneeFullName,
                SQLText oldAssigneeEmail,
                SQLText oldSummary,
                SQLText newStatus,
                SQLText newResolution,
                SQLText newModule,
                SQLText newSeverity,
                SQLText newPriority,
                SQLText newAssigneeFullName,
                SQLText newAssigneeEmail,
                SQLText newSummary]
    = "<div class=\"edit\">\n"
      ++ "At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
      ++ "<a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
      ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a> "
      ++ "changed "
      ++ (intercalate ", "
             $ concat [if statusChanged /= 0
                          then ["<b>status</b> from " ++ (escapeHTML oldStatus)
                                ++ " to " ++ (escapeHTML newStatus)]
                          else [],
                       if resolutionChanged /= 0
                          then ["<b>resolution</b> from " ++ (escapeHTML oldResolution)
                                ++ " to " ++ (escapeHTML newResolution)]
                          else [],
                       if moduleChanged /= 0
                          then ["<b>module</b> from " ++ (escapeHTML oldModule)
                                ++ " to " ++ (escapeHTML newModule)]
                          else [],
                       if severityChanged /= 0
                          then ["<b>severity</b> from " ++ (escapeHTML oldSeverity)
                                ++ " to " ++ (escapeHTML newSeverity)]
                          else [],
                       if priorityChanged /= 0
                          then ["<b>priority</b> from " ++ (escapeHTML oldPriority)
                                ++ " to " ++ (escapeHTML newPriority)]
                          else [],
                       if assigneeChanged /= 0
                          then ["<b>assignee</b> from "
                                ++ "<a href=\"mailto:"
                                ++ (escapeAttribute oldAssigneeEmail)
                                ++ "\">" ++ (escapeHTML oldAssigneeFullName)
                                ++ " &lt;" ++ (escapeHTML oldAssigneeEmail)
                                ++ "&gt;</a>"
                                ++ " to "
                                ++ "<a href=\"mailto:"
                                ++ (escapeAttribute newAssigneeEmail)
                                ++ "\">" ++ (escapeHTML newAssigneeFullName)
                                ++ " &lt;" ++ (escapeHTML newAssigneeEmail)
                                ++ "&gt;</a>"]
                          else [],
                       if summaryChanged /= 0
                          then ["<b>summary</b> from " ++ (escapeHTML oldSummary)
                                ++ " to " ++ (escapeHTML newSummary)]
                          else []])
      ++ ".\n"
      ++ "</div>\n"


viewCommentDetail :: [SQLData] -> String
viewCommentDetail [SQLInteger timestamp,
                   SQLText fullName,
                   SQLText email,
                   _,
                   SQLText text]
    = "<div class=\"comment\">\n"
      ++ "At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
      ++ "<a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
      ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a> "
      ++ "wrote:\n"
      ++ "<pre>" ++ (escapeHTML text) ++ "</pre>\n"
      ++ "</div>\n"


viewAttachmentDetail :: Int64 -> [SQLData] -> String
viewAttachmentDetail id [SQLInteger timestamp,
                         SQLText fullName,
                         SQLText email,
                         _,
                         SQLText filename,
                         SQLInteger size]
    = "<div class=\"attachment\">\n"
      ++ "At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
      ++ "<a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
      ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a> "
      ++ "attached \n"
      ++ "<a href=\"/issues/attachment/" ++ (show id) ++ "/"
      ++ (escapeAttribute filename) ++ "/\">"
      ++ (escapeHTML filename) ++ "</a> ("
      ++ (escapeHTML $ byteSizeToString size)
      ++ ").\n"
      ++ "</div>\n"


createGET :: Buglist CGIResult
createGET = do
  doNotCreateIssue 1 defaultSummary defaultComment defaultFullName defaultEmail
                   Nothing


createPOST :: Buglist CGIResult
createPOST = do
  maybeModuleID <- getInput "module"
  moduleID <- return $ case maybeModuleID of
               Just moduleID -> read moduleID
               Nothing -> 0
  maybeSummary <- getInput "summary"
  summary <- return $ case maybeSummary of
               Just summary -> fromCRLF summary
               Nothing -> ""
  maybeComment <- getInput "comment"
  comment <- return $ case maybeComment of
               Just comment -> fromCRLF comment
               Nothing -> ""
  maybeFullName <- getInput "full-name"
  fullName <- return $ case maybeFullName of
               Just "" -> defaultFullName
               Just fullName -> fromCRLF fullName
               Nothing -> defaultFullName
  maybeEmail <- getInput "email"
  email <- return $ case maybeEmail of
               Just "" -> defaultEmail
               Just email -> fromCRLF email
               Nothing -> defaultEmail
  if (summary == "") || (summary == defaultSummary) || (elem '\n' summary)
    then doNotCreateIssue moduleID summary comment fullName email
                          (Just "Please enter a one-line summary.")
    else if (comment == "") || (comment == defaultComment)
         then doNotCreateIssue moduleID summary comment fullName email
                               (Just "Please enter a description of the problem.")
         else actuallyCreateIssue moduleID summary comment fullName email


doNotCreateIssue :: Int64 -> String -> String -> String -> String -> Maybe String
                 -> Buglist CGIResult
doNotCreateIssue moduleID summary comment fullName email maybeWarning = do
  modules <- query "SELECT id, name FROM modules ORDER BY id" []
  output $ "<html><head>\n"
         ++ "<title>Report a Bug</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body\n"
         ++ "<h1>Report a Bug</h1>\n"
         ++ "<form method=\"POST\" action=\"/issues/create/\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ warning ++ "</div>\n"
              Nothing -> ""
         ++ "<b>Module:</b> <select name=\"module\">"
         ++ (concat $ map (\[SQLInteger id, SQLText name]
                            -> "<option "
                               ++ (if id == fromIntegral moduleID
                                      then "selected "
                                      else "")
                               ++ "value=\""
                               ++ (show id)
                               ++ "\">" ++ (escapeHTML name)
                               ++ "</option>")
                          modules)
         ++ "</select><br />\n"
         ++ "<b>Summary:</b> <input type=\"text\" size=\"40\" name=\"summary\" "
         ++ "value=\"" ++ summary ++ "\" /><br />\n"
         ++ "<b>Description:</b><br />\n"
         ++ "<textarea name=\"comment\" rows=\"30\" cols=\"50\">"
         ++ comment
         ++ "</textarea><br />\n"
         ++ "<b>Full Name:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"full-name\" value=\""
         ++ fullName
         ++ "\"/><br />\n"
         ++ "<b>Email:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"email\" value=\""
         ++ email
         ++ "\"/><br />\n"
         ++ "<input type=\"submit\" value=\"Report\" />\n"
         ++ "</form>\n"
         ++ "</body></html>"


actuallyCreateIssue :: Int64 -> String -> String -> String -> String -> Buglist CGIResult
actuallyCreateIssue moduleID summary comment fullName email = do
  reporterID <- getUser fullName email
  assigneeID <- getUser "Nobody" "nobody"
  timestamp <- getTimestamp
  [[SQLInteger defaultStatusID, SQLInteger defaultResolutionID,
    SQLInteger defaultSeverityID, SQLInteger defaultPriorityID]]
      <- query ("SELECT first(status), first(resolution), first(severity), "
                ++ "first(priority) FROM defaults")
  query ("INSERT INTO issues (status, resolution, severity, priority, module, assignee, "
         ++ "reporter, summary, timestamp_created, timestamp_modified) "
         ++ "VALUES (1, 1, 1, 1, ?, ?, ?, ?, ?, ?)")
        [SQLInteger defaultStatusID, SQLInteger defaultResolutionID,
         SQLInteger defaultSeverityID, SQLInteger defaultPriorityID,
         SQLInteger moduleID, SQLInteger assigneeID, SQLInteger reporterID,
         SQLText summary, SQLInteger timestamp, SQLInteger timestamp]
  [[SQLInteger issueID]] <- query ("SELECT id FROM issues WHERE reporter = ? "
                                   ++ "AND timestamp_created = ?")
                                  [SQLInteger reporterID, SQLInteger timestamp]
  query ("INSERT INTO user_issue_comments (user, issue, timestamp, text) "
         ++ "VALUES (?, ?, ?, ?)")
        [SQLInteger reporterID, SQLInteger issueID, SQLInteger timestamp,
         SQLText comment]
  seeOtherRedirect ("/issues/view/" ++ (show issueID) ++ "/")


comment :: Int64 -> Buglist CGIResult
comment issueID = do
  maybeComment <- getInput "comment"
  comment <- return $ case maybeComment of
               Just comment -> fromCRLF comment
               Nothing -> ""
  maybeFullName <- getInput "full-name"
  fullName <- return $ case maybeFullName of
               Just "" -> defaultFullName
               Just fullName -> fromCRLF fullName
               Nothing -> defaultFullName
  maybeEmail <- getInput "email"
  email <- return $ case maybeEmail of
               Just "" -> defaultEmail
               Just email -> fromCRLF email
               Nothing -> defaultEmail
  if (comment == "")
     then doNotCreateComment issueID comment fullName email
     else actuallyCreateComment issueID comment fullName email


doNotCreateComment :: Int64 -> String -> String -> String -> Buglist CGIResult
doNotCreateComment issueID comment fullName email = do
  output "Did not create comment."


actuallyCreateComment :: Int64 -> String -> String -> String -> Buglist CGIResult
actuallyCreateComment issueID comment fullName email = do
  output "Created comment."


getUser :: String -> String -> Buglist Int64
getUser fullName email = do
  rows <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
  case rows of
    [[SQLInteger id]] -> return id
    _ -> do
      query ("INSERT INTO users (full_name, email, password_hash, right_admin_users, "
             ++ "right_see_emails, right_report_issues, right_modify_issues, "
             ++ "right_upload_files, right_comment_issues) "
             ++ "VALUES (?, ?, NULL, 0, 0, 1, 0, 0, 1)")
            [SQLText fullName, SQLText email]
      [[SQLInteger id]] <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
      return id


defaultSummary :: String
defaultSummary = "Short description of the problem"


defaultComment :: String
defaultComment
   =  "Steps to Reproduce\n"
   ++ "==================\n"
   ++ "\n"
   ++ "\n"
   ++ "Expected Behavior\n"
   ++ "=================\n"
   ++ "\n"
   ++ "\n"
   ++ "Actual Result\n"
   ++ "=============\n"


defaultFullName :: String
defaultFullName = "Anonymous"


defaultEmail :: String
defaultEmail = "anonymous"

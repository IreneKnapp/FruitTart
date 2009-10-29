module Controller.Issues where

import Data.Int
import Data.List
import Network.FastCGI hiding (output)

import Buglist
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
                   ++ "ORDER BY issues.priority ASC, issues.timestamp_modified DESC")
                  []
  navigationBar' <- navigationBar "/issues/index/"
  output  $ "<html><head>\n"
         ++ "<title>Buglist Issues</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar'
         ++ "<h1>Buglist Issues</h1>\n"
         ++ "<table>\n"
         ++ "<tr><th>ID</th><th>Modified</th><th>Stat</th><th>Res</th>"
         ++ "<th>Module</th><th>Sev</th><th>Pri</th>"
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
                  ++ "<td>" ++ (escapeHTML $ timestampToString modified) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML status) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML resolution) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML module') ++ "</td>"
                  ++ "<td>" ++ (escapeHTML severity) ++ "</td>"
                  ++ "<td>" ++ (escapeHTML priority) ++ "</td>"
                  ++ "<td><a href=\"/issues/view/" ++ (show id) ++ "/\">"
                  ++ (escapeHTML summary) ++ "</a></td>"
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
       navigationBar' <- navigationBar $ "/issues/view/" ++ (show id) ++ "/"
       output
         $  "<html><head>\n"
         ++ "<title>" ++ (escapeHTML summary) ++ "</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar'
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
         ++ "<div class=\"form\">\n"
         ++ "<h2>Comment on this issue?</h2>\n"
         ++ "<form method=\"POST\" action=\"/issues/comment/" ++ (show id) ++ "/\">\n"
         ++ "<div><textarea class=\"code\" name=\"comment\" rows=\"30\" cols=\"50\">"
         ++ "</textarea></div>\n"
         ++ "<div><b>Full Name:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"full-name\" value=\""
         ++ defaultFullName
         ++ "\"/></div>\n"
         ++ "<div><b>Email:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"email\" value=\""
         ++ defaultEmail
         ++ "\"/></div>\n"
         ++ "<div><button type=\"submit\" value=\"Comment\">Comment</button></div>\n"
         ++ "</form>\n"
         ++ "</div>\n"
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
      ++ "<h2>At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
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
      ++ ".</h2>\n"
      ++ "</div>\n"


viewCommentDetail :: [SQLData] -> String
viewCommentDetail [SQLInteger timestamp,
                   SQLText fullName,
                   SQLText email,
                   _,
                   SQLText text]
    = "<div class=\"comment\">\n"
      ++ "<h2>At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
      ++ "<a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
      ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a> "
      ++ "wrote:</h2>\n"
      ++ "<div class=\"code\">\n" ++ (newlinesToParagraphs text) ++ "</div>\n"
      ++ "</div>\n"


viewAttachmentDetail :: Int64 -> [SQLData] -> String
viewAttachmentDetail id [SQLInteger timestamp,
                         SQLText fullName,
                         SQLText email,
                         _,
                         SQLText filename,
                         SQLInteger size]
    = "<div class=\"attachment\">\n"
      ++ "<h2>At " ++ (escapeHTML $ timestampToString timestamp) ++ ", "
      ++ "<a href=\"mailto:" ++ (escapeAttribute email) ++ "\">"
      ++ (escapeHTML fullName) ++ " &lt;" ++ (escapeHTML email) ++ "&gt;</a> "
      ++ "attached \n"
      ++ "<a href=\"/issues/attachment/" ++ (show id) ++ "/"
      ++ (escapeAttribute filename) ++ "/\">"
      ++ (escapeHTML filename) ++ "</a> ("
      ++ (escapeHTML $ byteSizeToString size)
      ++ ").</h2>\n"
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
  navigationBar' <- navigationBar "/issues/create/"
  output $ "<html><head>\n"
         ++ "<title>Report an Issue</title>\n"
         ++ "<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar'
         ++ "<h1>Report an Issue</h1>\n"
         ++ "<form method=\"POST\" action=\"/issues/create/\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ warning ++ "</div>\n"
              Nothing -> ""
         ++ "<div><b>Module:</b> <select name=\"module\">"
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
         ++ "</select></div>\n"
         ++ "<div><b>Summary:</b> <input type=\"text\" size=\"40\" name=\"summary\" "
         ++ "value=\"" ++ summary ++ "\" /></div>\n"
         ++ "<div><b>Description:</b><br />\n"
         ++ "<textarea class=\"code\" name=\"comment\" rows=\"30\" cols=\"50\">"
         ++ comment
         ++ "</textarea></div>\n"
         ++ "<div><b>Full Name:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"full-name\" value=\""
         ++ fullName
         ++ "\"/></div>\n"
         ++ "<div><b>Email:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"email\" value=\""
         ++ email
         ++ "\"/></div>\n"
         ++ "<div><button type=\"submit\" value=\"Report\">Report</button></div>\n"
         ++ "</form>\n"
         ++ "</body></html>"


actuallyCreateIssue :: Int64 -> String -> String -> String -> String -> Buglist CGIResult
actuallyCreateIssue moduleID summary comment fullName email = do
  reporterID <- getUser fullName email
  assigneeID <- getUser "Nobody" "nobody"
  timestamp <- getTimestamp
  [[SQLInteger defaultStatusID, SQLInteger defaultResolutionID,
    SQLInteger defaultSeverityID, SQLInteger defaultPriorityID]]
      <- query ("SELECT status, resolution, severity, priority "
                ++ "FROM defaults LIMIT 1")
               []
  query ("INSERT INTO issues (status, resolution, severity, priority, module, assignee, "
         ++ "reporter, summary, timestamp_created, timestamp_modified) "
         ++ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
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
  commenterID <- getUser fullName email
  timestamp <- getTimestamp
  query ("INSERT INTO user_issue_comments (user, issue, timestamp, text) "
         ++ "VALUES (?, ?, ?, ?)")
        [SQLInteger commenterID, SQLInteger issueID, SQLInteger timestamp,
         SQLText comment]
  seeOtherRedirect $ "/issues/view/" ++ (show issueID) ++ "/"


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

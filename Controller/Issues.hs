module Controller.Issues where

import Data.Int
import Data.List
import Data.Maybe
import Network.FastCGI hiding (output, logCGI)

import Buglist
import Controller.Captcha
import Controller.Login
import Database
import {-# SOURCE #-} Dispatcher
import HTML
import Lists
import SQLite3 (SQLData(..))
import Text

index :: Maybe String -> Maybe (Either String Int64) -> Buglist CGIResult
index maybeWhich maybeEitherModuleNameModuleID = do
  sessionID <- getSessionID
  [[maybeDatabaseWhich, maybeDatabaseAllModules, maybeDatabaseModuleID]]
    <- query ("SELECT issue_index_filter_which, issue_index_filter_all_modules, "
              ++ "issue_index_filter_module FROM sessions "
              ++ "WHERE id = ?")
             [SQLInteger sessionID]
  which <- return $ case maybeWhich of
             Nothing -> case maybeDatabaseWhich of
                          SQLText which | elem which ["open", "closed", "all"] -> which
                          _ -> "open"
             Just which | elem which ["open", "closed", "all"] -> which
                        | True -> "open"
  maybeDatabaseModuleID <- return $ case maybeDatabaseModuleID of
                             SQLInteger moduleID -> Just moduleID
                             _ -> Nothing
  maybeDatabaseModuleName
    <- case maybeDatabaseModuleID of
         Nothing -> return Nothing
         Just databaseModuleID -> do
             [[maybeDatabaseModuleName]] <- query "SELECT name FROM modules WHERE id = ?"
                                                  [SQLInteger databaseModuleID]
             case maybeDatabaseModuleName of
               SQLText databaseModuleName -> return $ Just databaseModuleName
               _ -> return Nothing
  maybeModuleID <- return $ case maybeEitherModuleNameModuleID of
                     Nothing -> case maybeDatabaseAllModules of
                                  SQLInteger 1 -> Nothing
                                  _ -> maybeDatabaseModuleID
                     Just (Left "all") -> Nothing
                     Just (Left _) -> maybeDatabaseModuleID
                     Just (Right moduleID) -> Just moduleID
  maybeModuleName
      <- case maybeModuleID of
           Nothing -> return Nothing
           Just moduleID -> do
                [[SQLText moduleName]] <- query "SELECT name FROM modules WHERE id = ?"
                                                [SQLInteger moduleID]
                return $ Just moduleName
  query ("UPDATE sessions SET issue_index_filter_which = ?, "
         ++ "issue_index_filter_all_modules = ?, "
         ++ "issue_index_filter_module = ? "
         ++ "WHERE id = ?")
        [SQLText which,
         case maybeModuleID of
           Nothing -> SQLInteger 1
           Just _ -> SQLInteger 0,
         case maybeModuleID of
           Nothing -> case maybeDatabaseModuleID of
                        Nothing -> SQLNull
                        Just moduleID -> SQLInteger moduleID
           Just moduleID -> SQLInteger moduleID,
         SQLInteger sessionID]
  (maybeSubnavigationModuleID, maybeSubnavigationModuleName)
      <- return $ case maybeModuleID of
           Nothing -> (maybeDatabaseModuleID, maybeDatabaseModuleName)
           _ -> (maybeModuleID, maybeModuleName)
  whereClauseBody <- return
                     $ intercalate " AND "
                     $ concat [case which of
                                 _ | which == "open" -> ["statuses.name != 'CLOSED' "]
                                   | which == "closed" -> ["statuses.name == 'CLOSED' "]
                                   | which == "all" -> [],
                               case maybeModuleID of
                                 Nothing -> []
                                 Just moduleID -> ["module = ? "]]
  whereClause <- return $ if whereClauseBody == ""
                        then ""
                        else "WHERE " ++ whereClauseBody
  whereClauseParameters <- return $ case maybeModuleID of
                                      Nothing -> []
                                      Just moduleID -> [SQLInteger moduleID]
  reportNamePart1 <- return $ case which of
                   _ | which == "open" -> "Open Issues"
                     | which == "closed" -> "Closed Issues"
                     | which == "all" -> "All Issues"
  reportNamePart2 <- return $ case maybeModuleName of
                                Nothing -> "All Modules"
                                Just moduleName -> moduleName
  reportName <- return $ reportNamePart1 ++ " in " ++ reportNamePart2
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
                   ++ whereClause
                   ++ "ORDER BY issues.priority ASC, issues.timestamp_modified DESC")
                  ([] ++ whereClauseParameters)
  pageHeadItems <- getPageHeadItems
  currentPage <- return "/issues/index/"
  navigationBar <- getNavigationBar currentPage
  loginButton <- getLoginButton currentPage
  popupMessage <- getPopupMessage
  currentPathWhichPart <- return $ "which:" ++ which ++ "/"
  currentPathModulePart <- return $ case maybeModuleID of
                             Nothing -> "module:all/"
                             Just moduleID -> "module:" ++ (show moduleID) ++ "/"
  currentPath <- return $ "/issues/index/"
                        ++ currentPathWhichPart
                        ++ currentPathModulePart
  subnavigationBar <- getSubnavigationBar
                      currentPath
                      $ [Just ("All Issues",
                               "/issues/index/which:all/" ++ currentPathModulePart),
                         Just ("Open Issues",
                               "/issues/index/which:open/" ++ currentPathModulePart),
                         Just ("Closed Issues",
                               "/issues/index/which:closed/" ++ currentPathModulePart),
                         Nothing,
                         Just ("All Modules", "/issues/index/" ++ currentPathWhichPart
                               ++ "module:all/")]
                      ++ (case maybeSubnavigationModuleID of
                            Nothing -> []
                            Just moduleID
                                -> [Just (fromJust $ maybeSubnavigationModuleName,
                                          "/issues/index/"
                                          ++ currentPathWhichPart
                                          ++ "module:" ++ (show moduleID) ++ "/")])
  let filterItem name link
          = if currentPath == link
            then "<b>" ++ (escapeHTML name) ++ "</b>"
            else "<a href=\"" ++ (escapeAttribute link ) ++ "\">"
                 ++ (escapeHTML name) ++ "</a>"
  statusFilterList <- return $ map (\(name, link) -> filterItem name link)
                          [("All Issues",
                            "/issues/index/which:all/" ++ currentPathModulePart),
                           ("Open Issues",
                            "/issues/index/which:open/" ++ currentPathModulePart),
                           ("Closed Issues",
                            "/issues/index/which:closed/" ++ currentPathModulePart)]
  modules <- query "SELECT id, name FROM modules ORDER BY id" []
  modulesFilterList <- return $ [filterItem "All Modules"
                                      ("/issues/index/" ++ currentPathWhichPart
                                       ++ "module:all/")]
                                 ++ (map (\[SQLInteger id, SQLText name]
                                              -> filterItem
                                                   name
                                                   ("/issues/index/"
                                                    ++ currentPathWhichPart
                                                    ++ "module:" ++ (show id) ++ "/"))
                                           modules)
  output  $ "<html><head>\n"
         ++ "<title>" ++ reportName ++ "</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ subnavigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1>" ++ reportName ++ "</h1>\n"
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
         ++ "<h1>Filter Options</h1>\n"
         ++ "<ul>\n"
         ++ "<li><b>By Status</b><ul>\n"
         ++ (concat $ map (\item -> "<li>" ++ item ++ "</li>\n") statusFilterList)
         ++ "</ul></li>\n"
         ++ "<li><b>By Module</b><ul>\n"
         ++ (concat $ map (\item -> "<li>" ++ item ++ "</li>\n") modulesFilterList)
         ++ "</ul></li>\n"
         ++ "</ul>\n"
         ++ "</body></html>"


view :: Int64 -> Buglist CGIResult
view id = outputView id "" defaultFullName defaultEmail Nothing


outputView :: Int64 -> String -> String -> String -> (Maybe String) -> Buglist CGIResult
outputView id comment fullName email maybeWarning = do
  info <- query ("SELECT "
                 ++ "statuses.id,"
                 ++ "statuses.name, "
                 ++ "resolutions.id,"
                 ++ "resolutions.name, "
                 ++ "modules.id,"
                 ++ "modules.name, "
                 ++ "severities.id,"
                 ++ "severities.name, "
                 ++ "priorities.id,"
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
    [[SQLInteger statusID,
      SQLText status,
      SQLInteger resolutionID,
      SQLText resolution,
      SQLInteger moduleID,
      SQLText module',
      SQLInteger severityID,
      SQLText severity,
      SQLInteger priorityID,
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
       pageHeadItems <- getPageHeadItems
       currentPage <- return $ "/issues/view/" ++ (show id) ++ "/"
       navigationBar <- getNavigationBar currentPage
       loginButton <- getLoginButton currentPage
       popupMessage <- getPopupMessage
       statusPopup <- getStatusPopup $ Just statusID
       resolutionPopup <- getResolutionPopup $ Just resolutionID
       modulePopup <- getModulePopup $ Just moduleID
       severityPopup <- getSeverityPopup $ Just severityID
       priorityPopup <- getPriorityPopup $ Just priorityID
       userSelectionFormControls <- getUserSelectionFormControls fullName email
       captchaTimestamp <- generateCaptcha
       rightComment <- getRightCommentIssues
       commentForm <- return
         $  "<div class=\"form\">\n"
         ++ "<a name=\"comment\"><h2>Comment on this issue?</h2></a>\n"
         ++ "<form method=\"POST\" action=\"/issues/comment/"
         ++ (show id) ++ "/#comment\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ (escapeHTML warning)
                              ++ "</div>\n"
              Nothing -> ""
         ++ "<div><textarea class=\"code\" name=\"comment\" rows=\"30\" cols=\"50\">"
         ++ (escapeHTML comment)
         ++ "</textarea></div>\n"
         ++ userSelectionFormControls
         ++ "<div><b>The letters in this image:</b> "
         ++ "<img class=\"captcha\" src=\"/captcha/index/"
         ++ (show captchaTimestamp) ++ "/\"/> "
         ++ "<input type=\"text\" size=\"6\" name=\"captcha-response\" value=\"\"/>"
         ++ "<input type=\"hidden\" name=\"captcha-timestamp\" value=\""
         ++ (show captchaTimestamp) ++ "\"/>"
         ++ "</div>"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Comment\">Comment</button>"
         ++ "</div>\n"
         ++ "</form>\n"
         ++ "</div>\n"
       rightEdit <- getRightModifyIssues
       editForm <- return
         $  "<div class=\"form\">\n"
         ++ "<h2>Edit this issue?</h2>\n"
         ++ "<form method=\"POST\" action=\"/issues/edit/" ++ (show id) ++ "/\">\n"
         ++ "<div><b>Status:</b> " ++ statusPopup ++ "</div>\n"
         ++ "<div><b>Resolution:</b> " ++ resolutionPopup ++ "</div>\n"
         ++ "<div><b>Module:</b> " ++ modulePopup ++ "</div>\n"
         ++ "<div><b>Severity:</b> " ++ severityPopup ++ "</div>\n"
         ++ "<div><b>Priority:</b> " ++ priorityPopup ++ "</div>\n"
         ++ "<div><b>Assignee:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"assignee-email\" value=\""
         ++ (escapeAttribute assigneeEmail)
         ++ "\"/></div>\n"
         ++ "<div><b>Summary:</b> "
         ++ "<input type=\"text\" size=\"40\" name=\"summary\" value=\""
         ++ (escapeAttribute summary)
         ++ "\"/></div>\n"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Edit\">Edit</button>"
         ++ "</div>\n"
         ++ "</form>\n"
         ++ "</div>\n"
       output
         $  "<html><head>\n"
         ++ "<title>" ++ (escapeHTML summary) ++ "</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1>" ++ (escapeHTML summary) ++ "</h1>\n"
         ++ "<table class=\"issuestatus\">\n"
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
         ++ (if rightComment
               then commentForm
               else "")
         ++ (if rightEdit
               then editForm
               else "")
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
                          then ["<b>summary</b> from “" ++ (escapeHTML oldSummary)
                                ++ "” to “" ++ (escapeHTML newSummary) ++ "”"]
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
  right <- getRightReportIssues
  case right of
    False -> outputMustLoginPage "/issues/create/"
    True -> doNotCreateIssue 1 defaultSummary defaultComment defaultFullName defaultEmail
                             Nothing


createPOST :: Buglist CGIResult
createPOST = do
  right <- getRightReportIssues
  case right of
    False -> outputMustLoginPage "/issues/create/"
    True -> do
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
      (fullName, email) <- getFullNameAndEmail
      maybeCaptchaTimestamp <- getInput "captcha-timestamp"
      captchaTimestamp <- return $ case maybeCaptchaTimestamp of
                                     Just captchaTimestamp -> read captchaTimestamp
                                     Nothing -> 0
      maybeCaptchaResponse <- getInput "captcha-response"
      captchaResponse <- return $ case maybeCaptchaResponse of
                                    Just captchaResponse -> captchaResponse
                                    Nothing -> ""
      captchaValid <- checkCaptcha captchaTimestamp captchaResponse
      if (summary == "") || (summary == defaultSummary) || (elem '\n' summary)
        then doNotCreateIssue moduleID summary comment fullName email
                              (Just "Please enter a one-line summary.")
        else if (comment == "") || (comment == defaultComment)
             then doNotCreateIssue moduleID summary comment fullName email
                                   (Just "Please enter a description of the problem.")
             else if not captchaValid
                  then doNotCreateIssue moduleID summary comment fullName email
                                        (Just
                                         "Please enter the letters in the image below.")
                  else actuallyCreateIssue moduleID summary comment fullName email


doNotCreateIssue :: Int64 -> String -> String -> String -> String -> Maybe String
                 -> Buglist CGIResult
doNotCreateIssue moduleID summary comment fullName email maybeWarning = do
  pageHeadItems <- getPageHeadItems
  currentPage <- return "/issues/create/"
  navigationBar <- getNavigationBar currentPage
  loginButton <- getLoginButton currentPage
  popupMessage <- getPopupMessage
  modulePopup <- getModulePopup $ Just moduleID
  userSelectionFormControls <- getUserSelectionFormControls fullName email
  captchaTimestamp <- generateCaptcha
  output $ "<html><head>\n"
         ++ "<title>Report an Issue</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1>Report an Issue</h1>\n"
         ++ "<form method=\"POST\" action=\"/issues/create/\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ (escapeHTML warning)
                              ++ "</div>\n"
              Nothing -> ""
         ++ "<div><b>Module:</b> " ++ modulePopup ++ "</div>"
         ++ "<div><b>Summary:</b> <input type=\"text\" size=\"40\" name=\"summary\" "
         ++ "value=\"" ++ (escapeAttribute summary) ++ "\" /></div>\n"
         ++ "<div><b>Description:</b><br />\n"
         ++ "<textarea class=\"code\" name=\"comment\" rows=\"30\" cols=\"50\">"
         ++ (escapeHTML comment)
         ++ "</textarea></div>\n"
         ++ userSelectionFormControls
         ++ "<div><b>The letters in this image:</b> "
         ++ "<img class=\"captcha\" src=\"/captcha/index/"
         ++ (show captchaTimestamp) ++ "/\"/> "
         ++ "<input type=\"text\" size=\"6\" name=\"captcha-response\" value=\"\"/>"
         ++ "<input type=\"hidden\" name=\"captcha-timestamp\" value=\""
         ++ (show captchaTimestamp) ++ "\"/>"
         ++ "</div>"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Report\">Report</button>"
         ++ "</div>\n"
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
                ++ "FROM issue_defaults LIMIT 1")
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
  right <- getRightCommentIssues
  case right of
    False -> outputMustLoginPage $ "/issues/comment/" ++ (show issueID) ++ "/"
    True -> do
      maybeComment <- getInput "comment"
      comment <- return $ case maybeComment of
                   Just comment -> fromCRLF comment
                   Nothing -> ""
      (fullName, email) <- getFullNameAndEmail
      maybeCaptchaTimestamp <- getInput "captcha-timestamp"
      captchaTimestamp <- return $ case maybeCaptchaTimestamp of
                                     Just captchaTimestamp -> read captchaTimestamp
                                     Nothing -> 0
      maybeCaptchaResponse <- getInput "captcha-response"
      captchaResponse <- return $ case maybeCaptchaResponse of
                                    Just captchaResponse -> captchaResponse
                                    Nothing -> ""
      captchaValid <- checkCaptcha captchaTimestamp captchaResponse
      if (comment == "")
         then doNotCreateComment issueID comment fullName email "Please enter a comment!"
         else if not captchaValid
              then doNotCreateComment issueID comment fullName email
                                      "Please enter the letters in the image below."
              else actuallyCreateComment issueID comment fullName email


doNotCreateComment :: Int64 -> String -> String -> String -> String -> Buglist CGIResult
doNotCreateComment issueID comment fullName email warning = do
  outputView issueID comment fullName email (Just warning)


actuallyCreateComment :: Int64 -> String -> String -> String -> Buglist CGIResult
actuallyCreateComment issueID comment fullName email = do
  commenterID <- getUser fullName email
  timestamp <- getTimestamp
  query "BEGIN TRANSACTION" []
  query ("INSERT INTO user_issue_comments (user, issue, timestamp, text) "
         ++ "VALUES (?, ?, ?, ?)")
        [SQLInteger commenterID, SQLInteger issueID, SQLInteger timestamp,
         SQLText comment]
  query ("UPDATE issues SET timestamp_modified = ? WHERE id = ?")
        [SQLInteger timestamp,
         SQLInteger issueID]
  query "COMMIT" []
  setPopupMessage $ Just "Added comment."
  seeOtherRedirect $ "/issues/view/" ++ (show issueID) ++ "/"


edit :: Int64 -> Buglist CGIResult
edit issueID = do
  right <- getRightModifyIssues
  case right of
    False -> outputMustLoginPage $ "/issues/edit/" ++ (show issueID) ++ "/"
    True -> do
      maybeNewStatusID <- getInput "status"
      newStatusID <- return $ case maybeNewStatusID of
                                   Just newStatusID -> read newStatusID
                                   Nothing -> 1
      maybeNewResolutionID <- getInput "resolution"
      newResolutionID <- return $ case maybeNewResolutionID of
                                   Just newResolutionID -> read newResolutionID
                                   Nothing -> 1
      maybeNewModuleID <- getInput "module"
      newModuleID <- return $ case maybeNewModuleID of
                                   Just newModuleID -> read newModuleID
                                   Nothing -> 1
      maybeNewSeverityID <- getInput "severity"
      newSeverityID <- return $ case maybeNewSeverityID of
                                   Just newSeverityID -> read newSeverityID
                                   Nothing -> 1
      maybeNewPriorityID <- getInput "priority"
      newPriorityID <- return $ case maybeNewPriorityID of
                                   Just newPriorityID -> read newPriorityID
                                   Nothing -> 1
      maybeNewAssigneeEmail <- getInput "assignee-email"
      newAssigneeEmail <- return $ case maybeNewAssigneeEmail of
                                        Just newAssigneeEmail -> newAssigneeEmail
                                        Nothing -> "nobody"
      maybeNewSummary <- getInput "summary"
      newSummary <- return $ case maybeNewSummary of
                               Just newSummary -> newSummary
                               Nothing -> ""
      query "BEGIN TRANSACTION" []
      newAssigneeID <- getUser "" newAssigneeEmail
      [[SQLInteger oldStatusID,
        SQLInteger oldResolutionID,
        SQLInteger oldModuleID,
        SQLInteger oldSeverityID,
        SQLInteger oldPriorityID,
        SQLInteger oldAssigneeID,
        SQLText oldSummary]]
          <- query ("SELECT status, resolution, module, severity, priority, assignee, "
                    ++ "summary "
                    ++ "FROM issues WHERE id = ?")
                   [SQLInteger issueID]
      timestamp <- getTimestamp
      query ("INSERT INTO user_issue_changes (user, issue, timestamp, "
             ++ "status_changed, resolution_changed, module_changed, severity_changed, "
             ++ "priority_changed, assignee_changed, summary_changed, "
             ++ "old_status, old_resolution, old_module, old_severity, old_priority, "
             ++ "old_assignee, old_summary, "
             ++ "new_status, new_resolution, new_module, new_severity, new_priority, "
             ++ "new_assignee, new_summary) "
             ++ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
             ++ "?, ?, ?)")
            [SQLInteger 1,
             SQLInteger issueID,
             SQLInteger timestamp,
             SQLInteger (if newStatusID /= oldStatusID then 1 else 0),
             SQLInteger (if newResolutionID /= oldResolutionID then 1 else 0),
             SQLInteger (if newModuleID /= oldModuleID then 1 else 0),
             SQLInteger (if newSeverityID /= oldSeverityID then 1 else 0),
             SQLInteger (if newPriorityID /= oldPriorityID then 1 else 0),
             SQLInteger (if newAssigneeID /= oldAssigneeID then 1 else 0),
             SQLInteger (if newSummary /= oldSummary then 1 else 0),
             SQLInteger oldStatusID,
             SQLInteger oldResolutionID,
             SQLInteger oldModuleID,
             SQLInteger oldSeverityID,
             SQLInteger oldPriorityID,
             SQLInteger oldAssigneeID,
             SQLText oldSummary,
             SQLInteger newStatusID,
             SQLInteger newResolutionID,
             SQLInteger newModuleID,
             SQLInteger newSeverityID,
             SQLInteger newPriorityID,
             SQLInteger newAssigneeID,
             SQLText newSummary]
      query ("UPDATE issues SET status = ?, resolution = ?, module = ?, severity = ?, "
             ++ "priority = ?, assignee = ?, summary = ?, timestamp_modified = ? "
             ++ "WHERE id = ?")
            [SQLInteger newStatusID,
             SQLInteger newResolutionID,
             SQLInteger newModuleID,
             SQLInteger newSeverityID,
             SQLInteger newPriorityID,
             SQLInteger newAssigneeID,
             SQLText newSummary,
             SQLInteger timestamp,
             SQLInteger issueID]
      query "COMMIT" []
      setPopupMessage $ Just "Edited issue."
      seeOtherRedirect ("/issues/view/" ++ (show issueID) ++ "/")


getUserSelectionFormControls :: String -> String -> Buglist String
getUserSelectionFormControls providedFullName providedEmail = do
  maybeUserID <- getLoggedInUser
  (maybeFullName, maybeEmail) <- case maybeUserID of
     Nothing -> return (Nothing, Nothing)
     Just userID -> do
       [[SQLText maybeFullName, SQLText maybeEmail]]
           <- query "SELECT full_name, email FROM users WHERE id = ?"
                    [SQLInteger userID]
       return (Just maybeFullName, Just maybeEmail)
  return $ if (isJust maybeUserID)
           then ("")
           else ("<div><b>Full Name:</b> "
                 ++ "<input type=\"text\" size=\"30\" name=\"full-name\" value=\""
                 ++ (escapeAttribute providedFullName)
                 ++ "\"/></div>\n"
                 ++ "<div><b>Email:</b> "
                 ++ "<input type=\"text\" size=\"30\" name=\"email\" value=\""
                 ++ (escapeAttribute providedEmail)
                 ++ "\"/>"
                 ++ "<br />" ++ (escapeHTML privacyNote)
                 ++ "</div>\n")


getFullNameAndEmail :: Buglist (String, String)
getFullNameAndEmail = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Nothing -> do
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
      return (fullName, email)
    Just userID -> do
      [[SQLText fullName, SQLText email]]
          <- query ("SELECT full_name, email FROM users WHERE id = ?")
                   [SQLInteger userID]
      return (fullName, email)


defaultSummary :: String
defaultSummary = "Short description of the problem"


defaultComment :: String
defaultComment
   =  "STEPS TO REPRODUCE\n"
   ++ "\n"
   ++ "\n"
   ++ "EXPECTED BEHAVIOR\n"
   ++ "\n"
   ++ "\n"
   ++ "ACTUAL RESULT\n"

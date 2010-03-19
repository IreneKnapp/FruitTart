module Network.FruitTart.Buglist.Controller.Issues (
                                                    actionTable,
                                                    getStatusPopup,
                                                    getResolutionPopup,
                                                    getModulePopup,
                                                    getSeverityPopup,
                                                    getPriorityPopup,
                                                   )
    where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Network.FruitTart.Base
import Network.FruitTart.Buglist.View.Navigation
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Captcha.Controller.Captcha hiding (actionTable)


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [],
                        [("which", StringParameter),
                         ("module", EitherStringIDParameter)],
                        toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view),
                       ("create", "GET", [], [], toDyn createGET),
                       ("create", "POST", [], [], toDyn createPOST),
                       ("comment", "POST", [IDParameter], [], toDyn comment),
                       ("edit", "POST", [IDParameter], [], toDyn edit)]


index :: Maybe String -> Maybe (Either String Int64) -> FruitTart ()
index maybeWhich maybeEitherModuleNameModuleID = do
  rightViewModulesRequiringLogin <- getRightViewModulesRequiringLogin
  let modulesWhereClauseBody = if rightViewModulesRequiringLogin
                                 then ""
                                 else "modules.visible_only_when_logged_in = 0"
      modulesWhereClause = if modulesWhereClauseBody == ""
                             then ""
                             else "WHERE " ++ modulesWhereClauseBody
      modulesWhereClauseParameters = []
  sessionID <- getSessionID
  [[maybeDatabaseWhich, maybeDatabaseAllModules, maybeDatabaseModuleID]]
    <- query ("SELECT issue_index_filter_which, issue_index_filter_all_modules, "
              ++ "issue_index_filter_module FROM buglist_sessions "
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
             [[maybeDatabaseModuleName]]
                 <- query "SELECT name FROM buglist_modules WHERE id = ?"
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
                [[SQLText moduleName]]
                    <- query "SELECT name FROM buglist_modules WHERE id = ?"
                             [SQLInteger moduleID]
                return $ Just moduleName
  query (  "UPDATE buglist_sessions SET "
        ++ "issue_index_filter_which = ?, "
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
                                 _ | which == "open" -> ["statuses.name != 'CLOSED'"]
                                   | which == "closed" -> ["statuses.name == 'CLOSED'"]
                                   | which == "all" -> [],
                               case maybeModuleID of
                                 Nothing -> []
                                 Just moduleID -> ["module = ?"],
                               case modulesWhereClauseBody of
                                 "" -> []
                                 _ -> [modulesWhereClauseBody]]
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
          = Map.fromList [(("Templates", "item"),
                           TemplateString $ filterItem' name link)]
      filterItem' name link
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
  modules <- query ("SELECT id, name FROM buglist_modules AS modules\n"
                    ++ modulesWhereClause ++ "\n"
                    ++ "ORDER BY sort_order")
                   ([] ++ modulesWhereClauseParameters)
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
  bind "Templates" "pageTitle" reportName
  bind "Templates" "pageHeadItems" pageHeadItems
  bind "Templates" "navigationBar" navigationBar
  bind "Templates" "subnavigationBar" subnavigationBar
  bind "Templates" "loginButton" loginButton
  bind "Templates" "popupMessage" popupMessage
  bind "Buglist.Controller.Issues" "statusFilterList" statusFilterList
  bind "Buglist.Controller.Issues" "modulesFilterList" modulesFilterList
  bindQueryMultipleRows "Buglist.Controller.Issues" "rows"
                   [("id", TInt),
                    ("status", TString),
                    ("resolution", TString),
                    ("module", TString),
                    ("severity", TString),
                    ("priority", TString),
                    ("assignee", TString),
                    ("reporter", TString),
                    ("summary", TString),
                    ("created", TInt),
                    ("modified", TInt)]
                   (  "SELECT "
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
                   ++ "FROM buglist_issues AS issues "
                   ++ "INNER JOIN buglist_statuses AS statuses "
                   ++ "ON issues.status = statuses.id "
                   ++ "INNER JOIN buglist_resolutions AS resolutions "
                   ++ "ON issues.resolution = resolutions.id "
                   ++ "INNER JOIN buglist_modules AS modules "
                   ++ "ON issues.module = modules.id "
                   ++ "INNER JOIN buglist_severities AS severities "
                   ++ "ON issues.severity = severities.id "
                   ++ "INNER JOIN buglist_priorities AS priorities "
                   ++ "ON issues.priority = priorities.id "
                   ++ "INNER JOIN users AS assignee ON issues.assignee = assignee.id "
                   ++ "INNER JOIN users AS reporter ON issues.reporter = reporter.id "
                   ++ whereClause ++ " "
                   ++ "ORDER BY issues.priority ASC, issues.timestamp_modified DESC")
                   ([] ++ whereClauseParameters)
  pageContent <- getTemplate "Buglist.Controller.Issues" "index"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  fPutStr page


view :: Int64 -> FruitTart ()
view id = do
  rightViewIssue <- getRightViewIssue id
  case rightViewIssue of
    False -> outputMustLoginPage ("/issues/view/" ++ (show id) ++ "/")
    True -> outputView id "" defaultFullName defaultEmail Nothing


outputView :: Int64 -> String -> String -> String -> (Maybe String) -> FruitTart ()
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
                 ++ "FROM buglist_issues AS issues "
                 ++ "INNER JOIN buglist_statuses AS statuses "
                 ++ "ON issues.status = statuses.id "
                 ++ "INNER JOIN buglist_resolutions AS resolutions "
                 ++ "ON issues.resolution = resolutions.id "
                 ++ "INNER JOIN buglist_modules AS modules "
                 ++ "ON issues.module = modules.id "
                 ++ "INNER JOIN buglist_severities AS severities "
                 ++ "ON issues.severity = severities.id "
                 ++ "INNER JOIN buglist_priorities AS priorities "
                 ++ "ON issues.priority = priorities.id "
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
                         ++ "FROM buglist_user_issue_changes AS user_issue_changes "
                         ++ "INNER JOIN users "
                         ++ "ON user_issue_changes.user = users.id "
                         ++ "INNER JOIN buglist_issues AS issues "
                         ++ "ON user_issue_changes.issue = issues.id "
                         ++ "LEFT JOIN buglist_statuses AS old_status "
                         ++ "ON user_issue_changes.old_status = old_status.id "
                         ++ "LEFT JOIN buglist_resolutions AS old_resolution "
                         ++ "ON user_issue_changes.old_resolution = old_resolution.id "
                         ++ "LEFT JOIN buglist_modules AS old_module "
                         ++ "ON user_issue_changes.old_module = old_module.id "
                         ++ "LEFT JOIN buglist_severities AS old_severity "
                         ++ "ON user_issue_changes.old_severity = old_severity.id "
                         ++ "LEFT JOIN buglist_priorities AS old_priority "
                         ++ "ON user_issue_changes.old_priority = old_priority.id "
                         ++ "LEFT JOIN users AS old_assignee "
                         ++ "ON user_issue_changes.old_assignee = old_assignee.id "
                         ++ "LEFT JOIN buglist_statuses AS new_status "
                         ++ "ON user_issue_changes.new_status = new_status.id "
                         ++ "LEFT JOIN buglist_resolutions AS new_resolution "
                         ++ "ON user_issue_changes.new_resolution = new_resolution.id "
                         ++ "LEFT JOIN buglist_modules AS new_module "
                         ++ "ON user_issue_changes.new_module = new_module.id "
                         ++ "LEFT JOIN buglist_severities AS new_severity "
                         ++ "ON user_issue_changes.new_severity = new_severity.id "
                         ++ "LEFT JOIN buglist_priorities AS new_priority "
                         ++ "ON user_issue_changes.new_priority = new_priority.id "
                         ++ "LEFT JOIN users AS new_assignee "
                         ++ "ON user_issue_changes.new_assignee = new_assignee.id "
                         ++ "WHERE issues.id = ? "
                         ++ "ORDER BY user_issue_changes.timestamp")
                        [SQLInteger $ fromIntegral id]
                   >>= return . map
                                (convertRowToBindings "Buglist.Controller.Issues"
                                                      [("timestamp", TInt),
                                                       ("fullName", TString),
                                                       ("email", TString),
                                                       ("type", TString),
                                                       ("statusChanged", TBool),
                                                       ("resolutionChanged", TBool),
                                                       ("moduleChanged", TBool),
                                                       ("severityChanged", TBool),
                                                       ("priorityChanged", TBool),
                                                       ("assigneeChanged", TBool),
                                                       ("summaryChanged", TBool),
                                                       ("oldStatus", TString),
                                                       ("oldResolution", TString),
                                                       ("oldModule", TString),
                                                       ("oldSeverity", TString),
                                                       ("oldPriority", TString),
                                                       ("oldAssigneeFullName", TString),
                                                       ("oldAssigneeEmail", TString),
                                                       ("oldSummary", TString),
                                                       ("newStatus", TString),
                                                       ("newResolution", TString),
                                                       ("newModule", TString),
                                                       ("newSeverity", TString),
                                                       ("newPriority", TString),
                                                       ("newAssigneeFullName", TString),
                                                       ("newAssigneeEmail", TString),
                                                       ("newSummary", TString)])
       comments <- query ("SELECT "
                          ++ "user_issue_comments.timestamp, "
                          ++ "users.full_name, "
                          ++ "users.email, "
                          ++ "'Comment', "
                          ++ "user_issue_comments.text "
                          ++ "FROM buglist_user_issue_comments AS user_issue_comments "
                          ++ "INNER JOIN users "
                          ++ "ON user_issue_comments.user = users.id "
                          ++ "INNER JOIN buglist_issues AS issues "
                          ++ "ON user_issue_comments.issue = issues.id "
                          ++ "WHERE issues.id = ? "
                          ++ "ORDER BY user_issue_comments.timestamp")
                         [SQLInteger $ fromIntegral id]
                   >>= return . map
                                (convertRowToBindings "Buglist.Controller.Issues"
                                                      [("timestamp", TInt),
                                                       ("fullName", TString),
                                                       ("email", TString),
                                                       ("type", TString),
                                                       ("text", TString)])
       files <- query ("SELECT "
                       ++ "user_issue_attachments.timestamp, "
                       ++ "users.full_name, "
                       ++ "users.email, "
                       ++ "'Attachment', "
                       ++ "user_issue_attachments.filename, "
                       ++ "length(user_issue_attachments.data) AS size "
                       ++ "FROM buglist_user_issue_attachments AS user_issue_attachments "
                       ++ "INNER JOIN users "
                       ++ "ON user_issue_attachments.user = users.id "
                       ++ "INNER JOIN buglist_issues AS issues ON "
                       ++ "user_issue_attachments.issue = issues.id "
                       ++ "WHERE issues.id = ? "
                       ++ "ORDER BY user_issue_attachments.timestamp")
                      [SQLInteger $ fromIntegral id]
                >>= return . map
                             (convertRowToBindings "Buglist.Controller.Issues"
                                                   [("timestamp", TInt),
                                                    ("fullName", TString),
                                                    ("email", TString),
                                                    ("type", TString),
                                                    ("filename", TString),
                                                    ("size", TInt)])
       rows <- return $ mergeBy (\bindingsA bindingsB ->
                                  let key = ("Buglist.Controller.Issues", "timestamp")
                                      TemplateInteger a
                                          = fromJust $ Map.lookup key bindingsA
                                      TemplateInteger b
                                          = fromJust $ Map.lookup key bindingsB
                                  in compare a b)
                                [changes, comments, files]
       bind "Buglist.Controller.Issues" "rows" rows
       bind "Buglist.Controller.Issues" "id" id
       bind "Buglist.Controller.Issues" "comment" comment
       bind "Buglist.Controller.Issues" "maybeWarning" maybeWarning
       bind "Buglist.Controller.Issues" "status" status
       bind "Buglist.Controller.Issues" "resolution" resolution
       bind "Buglist.Controller.Issues" "module" module'
       bind "Buglist.Controller.Issues" "severity" severity
       bind "Buglist.Controller.Issues" "priority" priority
       bind "Buglist.Controller.Issues" "assigneeFullName" assigneeFullName
       bind "Buglist.Controller.Issues" "assigneeEmail" assigneeEmail
       bind "Buglist.Controller.Issues" "reporterFullName" reporterFullName
       bind "Buglist.Controller.Issues" "reporterEmail" reporterEmail
       bind "Buglist.Controller.Issues" "summary" summary
       bind "Buglist.Controller.Issues" "timestampCreated" timestampCreated
       bind "Buglist.Controller.Issues" "timestampModified" timestampModified
       bind "Templates" "pageTitle"
            $ "Issue " ++ (show id) ++ ": " ++ (escapeHTML summary)
       pageHeadItems <- getPageHeadItems
       bind "Templates" "pageHeadItems" pageHeadItems
       currentPage <- return $ "/issues/view/" ++ (show id) ++ "/"
       navigationBar <- getNavigationBar currentPage
       bind "Templates" "navigationBar" navigationBar
       loginButton <- getLoginButton currentPage
       bind "Templates" "loginButton" loginButton
       popupMessage <- getPopupMessage
       bind "Templates" "popupMessage" popupMessage
       statusPopup <- getStatusPopup $ Just statusID
       bind "Buglist.Controller.Issues" "statusPopup" statusPopup
       resolutionPopup <- getResolutionPopup $ Just resolutionID
       bind "Buglist.Controller.Issues" "resolutionPopup" resolutionPopup
       modulePopup <- getModulePopup $ Just moduleID
       bind "Buglist.Controller.Issues" "modulePopup" modulePopup
       severityPopup <- getSeverityPopup $ Just severityID
       bind "Buglist.Controller.Issues" "severityPopup" severityPopup
       priorityPopup <- getPriorityPopup $ Just priorityID
       bind "Buglist.Controller.Issues" "priorityPopup" priorityPopup
       userSelectionFormControls <- getUserSelectionFormControls fullName email
       bind "Buglist.Controller.Issues" "userSelectionFormControls"
            userSelectionFormControls
       captchaTimestamp <- generateCaptcha
       bind "Buglist.Controller.Issues" "captchaTimestamp" captchaTimestamp
       rightComment <- getRightCommentIssues
       bind "Buglist.Controller.Issues" "rightComment" rightComment
       rightEdit <- getRightModifyIssues
       bind "Buglist.Controller.Issues" "rightEdit" rightEdit
       pageContent <- getTemplate "Buglist.Controller.Issues" "view"
       bind "Templates" "pageContent" pageContent
       page <- getTemplate "Templates" "page"
       fPutStr page
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


createGET :: FruitTart ()
createGET = do
  right <- getRightReportIssues
  case right of
    False -> outputMustLoginPage "/issues/create/"
    True -> doNotCreateIssue 1 defaultSummary defaultComment defaultFullName defaultEmail
                             Nothing


createPOST :: FruitTart ()
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
                  else do
                      reporterID <- getOrCreateUserID fullName email ""
                      canActAsReporter <- getCanActAsUser reporterID
                      case canActAsReporter of
                        False -> doNotCreateIssue moduleID summary comment fullName email
                                                  (Just
                                                   "Please log in to post as that user.")
                        True -> actuallyCreateIssue moduleID summary comment reporterID


doNotCreateIssue :: Int64 -> String -> String -> String -> String -> Maybe String
                 -> FruitTart ()
doNotCreateIssue moduleID summary comment fullName email maybeWarning = do
  pageHeadItems <- getPageHeadItems
  currentPage <- return "/issues/create/"
  navigationBar <- getNavigationBar currentPage
  loginButton <- getLoginButton currentPage
  popupMessage <- getPopupMessage
  modulePopup <- getModulePopup $ Just moduleID
  userSelectionFormControls <- getUserSelectionFormControls fullName email
  captchaTimestamp <- generateCaptcha
  bind "Templates" "pageTitle" "Report an Issue"
  bind "Templates" "pageHeadItems" pageHeadItems
  bind "Templates" "navigationBar" navigationBar
  bind "Templates" "loginButton" loginButton
  bind "Templates" "popupMessage" popupMessage
  bind "Buglist.Controller.Issues" "maybeWarning" maybeWarning
  bind "Buglist.Controller.Issues" "modulePopup" modulePopup
  bind "Buglist.Controller.Issues" "summary" summary
  bind "Buglist.Controller.Issues" "comment" comment
  bind "Buglist.Controller.Issues" "userSelectionFormControls" userSelectionFormControls
  bind "Buglist.Controller.Issues" "captchaTimestamp" captchaTimestamp
  pageContent <- getTemplate "Buglist.Controller.Issues" "create"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  fPutStr page


actuallyCreateIssue :: Int64 -> String -> String -> Int64 -> FruitTart ()
actuallyCreateIssue moduleID summary comment reporterID = do
  assigneeID <- getOrCreateUserID "Nobody" "nobody" ""
  timestamp <- getTimestamp
  [[SQLInteger defaultStatusID, SQLInteger defaultResolutionID,
    SQLInteger defaultSeverityID, SQLInteger defaultPriorityID]]
      <- query ("SELECT status, resolution, severity, priority "
                ++ "FROM buglist_issue_defaults LIMIT 1")
               []
  query (  "INSERT INTO buglist_issues "
        ++ "(status, resolution, severity, priority, module, assignee, "
        ++ "reporter, summary, timestamp_created, timestamp_modified) "
        ++ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
        [SQLInteger defaultStatusID, SQLInteger defaultResolutionID,
         SQLInteger defaultSeverityID, SQLInteger defaultPriorityID,
         SQLInteger moduleID, SQLInteger assigneeID, SQLInteger reporterID,
         SQLText summary, SQLInteger timestamp, SQLInteger timestamp]
  [[SQLInteger issueID]] <- query ("SELECT id FROM buglist_issues WHERE reporter = ? "
                                   ++ "AND timestamp_created = ?")
                                  [SQLInteger reporterID, SQLInteger timestamp]
  query ("INSERT INTO buglist_user_issue_comments (user, issue, timestamp, text) "
         ++ "VALUES (?, ?, ?, ?)")
        [SQLInteger reporterID, SQLInteger issueID, SQLInteger timestamp,
         SQLText comment]
  seeOtherRedirect ("/issues/view/" ++ (show issueID) ++ "/")


comment :: Int64 -> FruitTart ()
comment issueID = do
  rightViewIssue <- getRightViewIssue issueID
  case rightViewIssue of
    False -> outputMustLoginPage ("/issues/comment/" ++ (show issueID) ++ "/")
    True -> comment' issueID


comment' :: Int64 -> FruitTart ()
comment' issueID = do
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
              else do
                commenterID <- getOrCreateUserID fullName email ""
                canActAsCommenter <- getCanActAsUser commenterID
                case canActAsCommenter of
                  False -> doNotCreateComment issueID comment fullName email
                                              "Please log in to post as that user."
                  True -> actuallyCreateComment issueID comment commenterID


doNotCreateComment :: Int64 -> String -> String -> String -> String -> FruitTart ()
doNotCreateComment issueID comment fullName email warning = do
  outputView issueID comment fullName email (Just warning)


actuallyCreateComment :: Int64 -> String -> Int64 -> FruitTart ()
actuallyCreateComment issueID comment commenterID = do
  timestamp <- getTimestamp
  query "BEGIN TRANSACTION" []
  query ("INSERT INTO buglist_user_issue_comments (user, issue, timestamp, text) "
         ++ "VALUES (?, ?, ?, ?)")
        [SQLInteger commenterID, SQLInteger issueID, SQLInteger timestamp,
         SQLText comment]
  query ("UPDATE buglist_issues SET timestamp_modified = ? WHERE id = ?")
        [SQLInteger timestamp,
         SQLInteger issueID]
  query "COMMIT" []
  setPopupMessage $ Just "Added comment."
  seeOtherRedirect $ "/issues/view/" ++ (show issueID) ++ "/"


edit :: Int64 -> FruitTart ()
edit issueID = do
  rightViewIssue <- getRightViewIssue issueID
  case rightViewIssue of
    False -> outputMustLoginPage $ "/issues/edit/" ++ (show issueID) ++ "/"
    True -> edit' issueID


edit' :: Int64 -> FruitTart ()
edit' issueID = do
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
      newAssigneeID <- getOrCreateUserID "" newAssigneeEmail ""
      [[SQLInteger oldStatusID,
        SQLInteger oldResolutionID,
        SQLInteger oldModuleID,
        SQLInteger oldSeverityID,
        SQLInteger oldPriorityID,
        SQLInteger oldAssigneeID,
        SQLText oldSummary]]
          <- query ("SELECT status, resolution, module, severity, priority, assignee, "
                    ++ "summary "
                    ++ "FROM buglist_issues WHERE id = ?")
                   [SQLInteger issueID]
      timestamp <- getTimestamp
      query (  "INSERT INTO buglist_user_issue_changes "
            ++ "(user, issue, timestamp, "
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
      query (  "UPDATE buglist_issues "
            ++ "SET status = ?, resolution = ?, module = ?, severity = ?, "
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


getUserSelectionFormControls :: String -> String -> FruitTart String
getUserSelectionFormControls providedFullName providedEmail = do
  maybeUserID <- getLoggedInUserID
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


getFullNameAndEmail :: FruitTart (String, String)
getFullNameAndEmail = do
  maybeUserID <- getLoggedInUserID
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


getStatusPopup :: Maybe Int64 -> FruitTart String
getStatusPopup maybeStatusID = do
  statuses <- query "SELECT id, name FROM buglist_statuses ORDER BY id" []
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
  resolutions <- query "SELECT id, name FROM buglist_resolutions ORDER BY id" []
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
  rightViewModulesRequiringLogin <- getRightViewModulesRequiringLogin
  let whereClauseBody = if rightViewModulesRequiringLogin
                          then ""
                          else "modules.visible_only_when_logged_in = 0"
      whereClause = if whereClauseBody == ""
                      then ""
                      else "WHERE " ++ whereClauseBody
      whereClauseParameters = []
  modules <- query ("SELECT id, name FROM buglist_modules AS modules "
                    ++ whereClause ++ " "
                    ++ "ORDER BY sort_order")
                   ([] ++ whereClauseParameters)
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
  severities <- query "SELECT id, name FROM buglist_severities ORDER BY id" []
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
  priorities <- query "SELECT id, name FROM buglist_priorities ORDER BY id" []
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

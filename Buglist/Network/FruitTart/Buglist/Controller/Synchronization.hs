module Network.FruitTart.Buglist.Controller.Synchronization (actionTable)
    where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map, concat, index)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List

import Network.FruitTart.Base
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Buglist.View.Navigation
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("now", "GET", [], [], [], toDyn now),
                       ("index", "GET", [IDParameter], [], [], toDyn index),
                       ("issue", "GET", [IDParameter], [], [], toDyn issueGET),
                       ("issue", "POST", [IDParameter], [], [], toDyn issuePOST),
                       ("user-issue-change", "GET",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueChangeGET),
                       ("user-issue-change", "POST",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueChangePOST),
                       ("user-issue-comment", "GET",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueCommentGET),
                       ("user-issue-comment", "POST",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueCommentPOST),
                       ("user-issue-attachment", "GET",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueAttachmentGET),
                       ("user-issue-attachment", "POST",
                        [IDParameter, IDParameter, IDParameter], [], [],
                        toDyn userIssueAttachmentPOST)]


now :: FruitTart ()
now = do
  right <- getRightSynchronize
  case right of
    False -> do
        defaultPage <- getTemplate "Base" "defaultPage" []
        seeOtherRedirect defaultPage
    True -> now'


index :: Int64 -> FruitTart ()
index startTimestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> index' startTimestamp


issueGET :: Int64 -> FruitTart ()
issueGET issueID = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> issueGET' issueID


userIssueChangeGET :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueChangeGET userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueChangeGET' userID issueID timestamp


userIssueCommentGET :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueCommentGET userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueCommentGET' userID issueID timestamp


userIssueAttachmentGET :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueAttachmentGET userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueAttachmentGET' userID issueID timestamp


issuePOST :: Int64 -> FruitTart ()
issuePOST issueID = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> issuePOST' issueID


userIssueChangePOST :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueChangePOST userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueChangePOST' userID issueID timestamp


userIssueCommentPOST :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueCommentPOST userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueCommentPOST' userID issueID timestamp


userIssueAttachmentPOST :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueAttachmentPOST userID issueID timestamp = do
  right <- getRightSynchronize
  case right of
    False -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    True -> userIssueAttachmentPOST' userID issueID timestamp


now' :: FruitTart ()
now' = do
  fPutStr "Testing."


index' :: Int64 -> FruitTart ()
index' startTimestamp = do
  users <- query "SELECT id, full_name, email FROM users ORDER BY id"
                 []
  issues <- query (  "SELECT id, reporter, timestamp_created "
                  ++ "FROM buglist_issues WHERE timestamp_modified >= ? ORDER BY id")
                  [SQLInteger startTimestamp]
  userIssueChanges <- query (  "SELECT user, issue, timestamp "
                            ++ "FROM buglist_user_issue_changes "
                            ++ "WHERE timestamp >= ? "
                            ++ "ORDER BY timestamp, issue, user")
                            [SQLInteger startTimestamp]
  userIssueComments <- query (  "SELECT user, issue, timestamp "
                             ++ "FROM buglist_user_issue_comments "
                             ++ "WHERE timestamp >= ? "
                             ++ "ORDER BY timestamp, issue, user")
                             [SQLInteger startTimestamp]
  userIssueAttachments <- query (  "SELECT user, issue, timestamp "
                                ++ "FROM buglist_user_issue_attachments "
                                ++ "WHERE timestamp >= ? "
                                ++ "ORDER BY timestamp, issue, user")
                                [SQLInteger startTimestamp]
  setResponseHeader HttpContentType "text/xml; charset=UTF8"
  fPutStr $ "<buglist>\n"
         ++ "<users>\n"
         ++ (concat
             $ map (\[SQLInteger id,
                      SQLText fullName,
                      SQLText email] ->
                      "<user>"
                      ++ (integerTag "id" id)
                      ++ (textTag "full_name" fullName)
                      ++ (textTag "email" email)
                      ++ "</user>\n")
                   users)
         ++ "</users>\n"
         ++ "<issues>\n"
         ++ (concat
             $ map (\[SQLInteger id, SQLInteger reporter, SQLInteger timestampCreated] ->
                      "<issue>"
                      ++ (integerTag "id" id)
                      ++ (integerTag "reporter" reporter)
                      ++ (integerTag "timestamp_created" timestampCreated)
                      ++ "</issue>\n")
                   issues)
         ++ "</issues>\n"
         ++ "<user_issue_changes>\n"
         ++ (concat
             $ map (\[SQLInteger user, SQLInteger issue, SQLInteger timestamp] ->
                      "<user_issue_change>"
                      ++ (integerTag "user" user)
                      ++ (integerTag "issue" issue)
                      ++ (integerTag "timestamp" timestamp)
                      ++ "</user_issue_change>\n")
                   userIssueChanges)
         ++ "</user_issue_changes>\n"
         ++ "<user_issue_comments>\n"
         ++ (concat
             $ map (\[SQLInteger user, SQLInteger issue, SQLInteger timestamp] ->
                      "<user_issue_comment>"
                      ++ (integerTag "user" user)
                      ++ (integerTag "issue" issue)
                      ++ (integerTag "timestamp" timestamp)
                      ++ "</user_issue_comment>\n")
                   userIssueComments)
         ++ "</user_issue_comments>\n"
         ++ "<user_issue_attachments>\n"
         ++ (concat
             $ map (\[SQLInteger user, SQLInteger issue, SQLInteger timestamp] ->
                      "<user_issue_attachment>"
                      ++ (integerTag "user" user)
                      ++ (integerTag "issue" issue)
                      ++ (integerTag "timestamp" timestamp)
                      ++ "</user_issue_attachment>\n")
                   userIssueAttachments)
         ++ "</user_issue_attachments>\n"
         ++ "</buglist>\n"


issueGET' :: Int64 -> FruitTart ()
issueGET' issueID = do
  [[SQLInteger id,
    SQLInteger status,
    SQLInteger resolution,
    SQLInteger module',
    SQLInteger severity,
    SQLInteger priority,
    SQLInteger assignee,
    SQLInteger reporter,
    SQLText summary,
    SQLInteger timestampCreated,
    SQLInteger timestampModified]]
      <- query (  "SELECT id, status, resolution, module, severity, priority, assignee, "
               ++ "reporter, summary, timestamp_created, timestamp_modified "
               ++ "FROM buglist_issues WHERE id = ?")
               [SQLInteger issueID]
  setResponseHeader HttpContentType "text/xml; charset=UTF8"
  fPutStr $ "<issue>"
         ++ (integerTag "id" id)
         ++ (integerTag "status" status)
         ++ (integerTag "resolution" resolution)
         ++ (integerTag "module" module')
         ++ (integerTag "severity" severity)
         ++ (integerTag "priority" priority)
         ++ (integerTag "assignee" assignee)
         ++ (integerTag "reporter" reporter)
         ++ (textTag "summary" summary)
         ++ (integerTag "timestamp_created" timestampCreated)
         ++ (integerTag "timestamp_modified" timestampModified)
         ++ "</issue>\n"


userIssueChangeGET' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueChangeGET' userID issueID timestamp = do
  [[SQLInteger statusChanged,
    SQLInteger resolutionChanged,
    SQLInteger moduleChanged,
    SQLInteger severityChanged,
    SQLInteger priorityChanged,
    SQLInteger assigneeChanged,
    SQLInteger summaryChanged,
    SQLInteger oldStatus,
    SQLInteger oldResolution,
    SQLInteger oldModule,
    SQLInteger oldSeverity,
    SQLInteger oldPriority,
    SQLInteger oldAssignee,
    SQLText oldSummary,
    SQLInteger newStatus,
    SQLInteger newResolution,
    SQLInteger newModule,
    SQLInteger newSeverity,
    SQLInteger newPriority,
    SQLInteger newAssignee,
    SQLText newSummary]]
      <- query (  "SELECT status_changed, resolution_changed, module_changed, "
               ++ "severity_changed, priority_changed, assignee_changed, "
               ++ "summary_changed, old_status, old_resolution, old_module, "
               ++ "old_severity, old_priority, old_assignee, old_summary, "
               ++ "new_status, new_resolution, new_module, new_severity, "
               ++ "new_priority, new_assignee, new_summary "
               ++ "FROM buglist_user_issue_changes "
               ++ "WHERE user = ? AND issue = ? AND timestamp = ?")
               [SQLInteger userID, SQLInteger issueID, SQLInteger timestamp]
  setResponseHeader HttpContentType "text/xml; charset=UTF8"
  fPutStr $ "<user_issue_change>"
         ++ (integerTag "status_changed" statusChanged)
         ++ (integerTag "resolution_changed" resolutionChanged)
         ++ (integerTag "module_changed" moduleChanged)
         ++ (integerTag "severity_changed" severityChanged)
         ++ (integerTag "priority_changed" priorityChanged)
         ++ (integerTag "assignee_changed" assigneeChanged)
         ++ (integerTag "summary_changed" summaryChanged)
         ++ (integerTag "old_status" oldStatus)
         ++ (integerTag "old_resolution" oldResolution)
         ++ (integerTag "old_module" oldModule)
         ++ (integerTag "old_severity" oldSeverity)
         ++ (integerTag "old_priority" oldPriority)
         ++ (integerTag "old_assignee" oldAssignee)
         ++ (textTag "old_summary" oldSummary)
         ++ (integerTag "new_status" newStatus)
         ++ (integerTag "new_resolution" newResolution)
         ++ (integerTag "new_module" newModule)
         ++ (integerTag "new_severity" newSeverity)
         ++ (integerTag "new_priority" newPriority)
         ++ (integerTag "new_assignee" newAssignee)
         ++ (textTag "new_summary" newSummary)
         ++ "</user_issue_change>\n"


userIssueCommentGET' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueCommentGET' userID issueID timestamp = do
  [[SQLText text]]
      <- query (  "SELECT text "
               ++ "FROM buglist_user_issue_comments "
               ++ "WHERE user = ? AND issue = ? AND timestamp = ?")
               [SQLInteger userID, SQLInteger issueID, SQLInteger timestamp]
  setResponseHeader HttpContentType "text/xml; charset=UTF8"
  fPutStr $ "<user_issue_comment>"
         ++ (textTag "text" text)
         ++ "</user_issue_comment>\n"


userIssueAttachmentGET' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueAttachmentGET' userID issueID timestamp = do
  fPutStr $ ""


issuePOST' :: Int64 -> FruitTart ()
issuePOST' issueID = do
  fPutStr $ ""


userIssueChangePOST' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueChangePOST' userID issueID timestamp = do
  fPutStr $ ""


userIssueCommentPOST' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueCommentPOST' userID issueID timestamp = do
  fPutStr $ ""


userIssueAttachmentPOST' :: Int64 -> Int64 -> Int64 -> FruitTart ()
userIssueAttachmentPOST' userID issueID timestamp = do
  fPutStr $ ""


integerTag :: String -> Int64 -> String
integerTag name value = "<" ++ name ++ " value=\"" ++ (show value) ++ "\"/>"


textTag :: String -> String -> String
textTag name value = "<" ++ name ++ " value=\"" ++ (escapeAttribute value) ++ "\"/>"

module Controller.XML where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map, concat)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List
import Network.FastCGI hiding (output)

import Buglist
import Database
import {-# SOURCE #-} Dispatcher
import HTML
import Lists
import Passwords
import SQLite3 (SQLData(..))
import Text
import Types


get :: Buglist CGIResult
get = do
  users <- query "SELECT id, full_name, email FROM users ORDER BY id"
                 []
  issues <- query ("SELECT id, reporter, timestamp_created "
                   ++ "FROM issues ORDER BY id")
                  []
  userIssueChanges <- query ("SELECT user, issue, timestamp "
                             ++ "FROM user_issue_changes "
                             ++ "ORDER BY timestamp, issue, user")
                            []
  userIssueComments <- query ("SELECT user, issue, timestamp "
                              ++ "FROM user_issue_comments "
                              ++ "ORDER BY timestamp, issue, user")
                             []
  userIssueAttachments <- query ("SELECT user, issue, timestamp "
                                 ++ "FROM user_issue_attachments "
                                 ++ "ORDER BY timestamp, issue, user")
                                []
  setHeader "Content-Type" "text/xml; charset=UTF8"
  output $ "<buglist>\n"
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


post :: Buglist CGIResult
post = do
  setHeader "Content-Type" "text/xml; charset=UTF8"
  output ""


integerTag :: String -> Int64 -> String
integerTag name value = "<" ++ name ++ " value=\"" ++ (show value) ++ "\"/>"


textTag :: String -> String -> String
textTag name value = "<" ++ name ++ " value=\"" ++ (escapeAttribute value) ++ "\"/>"

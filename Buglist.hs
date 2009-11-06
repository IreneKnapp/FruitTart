module Buglist (
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

import qualified Database.SQLite3 as SQL
import Network.FruitTart.Dispatcher
import Network.FruitTart.Util






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

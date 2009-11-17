module Network.FruitTart.Buglist.Controller.Users (
                                                   actionTable,
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
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view)]


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
  bindNamedQueryMultipleRows "Buglist.Controller.Users" "indexRows" []
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
       bind "Buglist.Controller.Users" "userID" id
       bindNamedQuery "Buglist.Controller.Users" "userIssueActions" []
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

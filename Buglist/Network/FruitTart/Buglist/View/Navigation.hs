module Network.FruitTart.Buglist.View.Navigation (getSubnavigationBar,
                                                  getRightSynchronize,
                                                  getRightAdminUsers,
                                                  getRightSeeEmails,
                                                  getRightReportIssues,
                                                  getRightModifyIssues,
                                                  getRightUploadFiles,
                                                  getRightCommentIssues,
                                                  getRightViewModulesRequiringLogin,
                                                  getRightViewIssue
                                                 )
    where

import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login


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


getRightViewModulesRequiringLogin :: FruitTart Bool
getRightViewModulesRequiringLogin = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> return False
    Just _ -> return True


getRightViewIssue :: Int64 -> FruitTart Bool
getRightViewIssue issueID = do
  [[SQLInteger requiresLogin]]
      <- query ("SELECT modules.visible_only_when_logged_in "
                ++ "FROM buglist_issues AS issues LEFT JOIN buglist_modules AS modules "
                ++ "ON issues.module = modules.id "
                ++ "WHERE issues.id = ?")
               [SQLInteger issueID]
  case requiresLogin of
    0 -> return True
    _ -> getRightViewModulesRequiringLogin


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

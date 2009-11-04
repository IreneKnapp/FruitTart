module Buglist (main,
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

import Data.Int

import Types


main :: IO ()
getUser :: String -> String -> Buglist Int64
getLoggedInUser :: Buglist (Maybe Int64)
getEffectiveUser :: Buglist Int64
getCanActAsUser :: Int64 -> Buglist Bool
getRightSynchronize :: Buglist Bool
getRightAdminUsers :: Buglist Bool
getRightSeeEmails :: Buglist Bool
getRightReportIssues :: Buglist Bool
getRightModifyIssues :: Buglist Bool
getRightUploadFiles :: Buglist Bool
getRightCommentIssues :: Buglist Bool
getPageHeadItems :: Buglist String
getNavigationBar :: String -> Buglist String
getLoginButton :: String -> Buglist String
setPopupMessage :: Maybe String -> Buglist ()
getPopupMessage :: Buglist String
getSubnavigationBar :: String -> [Maybe (String, String)] -> Buglist String
getStatusPopup :: Maybe Int64 -> Buglist String
getResolutionPopup :: Maybe Int64 -> Buglist String
getModulePopup :: Maybe Int64 -> Buglist String
getSeverityPopup :: Maybe Int64 -> Buglist String
getPriorityPopup :: Maybe Int64 -> Buglist String
privacyNote :: String
defaultFullName :: String
defaultEmail :: String

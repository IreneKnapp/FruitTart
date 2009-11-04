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

import Network.FruitTart.Util.Types


main :: IO ()
getUser :: String -> String -> FruitTart Int64
getLoggedInUser :: FruitTart (Maybe Int64)
getEffectiveUser :: FruitTart Int64
getCanActAsUser :: Int64 -> FruitTart Bool
getRightSynchronize :: FruitTart Bool
getRightAdminUsers :: FruitTart Bool
getRightSeeEmails :: FruitTart Bool
getRightReportIssues :: FruitTart Bool
getRightModifyIssues :: FruitTart Bool
getRightUploadFiles :: FruitTart Bool
getRightCommentIssues :: FruitTart Bool
getPageHeadItems :: FruitTart String
getNavigationBar :: String -> FruitTart String
getLoginButton :: String -> FruitTart String
setPopupMessage :: Maybe String -> FruitTart ()
getPopupMessage :: FruitTart String
getSubnavigationBar :: String -> [Maybe (String, String)] -> FruitTart String
getStatusPopup :: Maybe Int64 -> FruitTart String
getResolutionPopup :: Maybe Int64 -> FruitTart String
getModulePopup :: Maybe Int64 -> FruitTart String
getSeverityPopup :: Maybe Int64 -> FruitTart String
getPriorityPopup :: Maybe Int64 -> FruitTart String
privacyNote :: String
defaultFullName :: String
defaultEmail :: String

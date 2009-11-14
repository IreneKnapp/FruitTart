module Network.FruitTart.Base.Controller.Login (actionTable)
    where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List

import Network.FruitTart.Base
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates


actionTable :: ActionTable
actionTable
    = makeActionTable [("login", "GET", [], [], toDyn loginGET),
                       ("login", "POST", [], [], toDyn loginPOST),
                       ("logout", "GET", [], [], toDyn logout),
                       ("account", "GET", [], [], toDyn accountGET),
                       ("account", "POST", [], [], toDyn accountPOST),
                       ("password", "GET", [], [], toDyn passwordGET),
                       ("password", "POST", [], [], toDyn passwordPOST)]


loginGET :: FruitTart CGIResult
loginGET = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
             sessionID <- getSessionID
             [[maybeRecentUserEmail]]
                 <- namedQuery "Base.Controller.Login" "recentUser"
                               [SQLInteger sessionID]
             maybeRecentUserEmail <- return $ case maybeRecentUserEmail of
                                                SQLNull -> Nothing
                                                SQLText email -> Just email
             doNotLogIn Nothing maybeRecentUserEmail
    Just _ -> do
             targetPage <- getReferrer
             seeOtherRedirect targetPage


loginPOST :: FruitTart CGIResult
loginPOST = do
  maybeEmail <- getInput "email"
  email <- return $ case maybeEmail of
                      Just email -> email
                      Nothing -> ""
  maybePassword <- getInput "password"
  password <- return $ case maybePassword of
                         Just password -> password
                         Nothing -> ""
  maybeUserIDRows <- namedQuery "Base.Controller.Login" "userByEmail" [SQLText email]
  case maybeUserIDRows of
    [] -> doNotLogIn (Just "Incorrect information.")
                     maybeEmail
    [[SQLInteger userID]] -> do
      valid <- validatePassword userID password
      case valid of
        True -> do
          sessionID <- getSessionID
          namedQuery "Base.Controller.Login" "logIn"
                     [SQLInteger userID, SQLInteger userID, SQLInteger sessionID]
          referrer <- getReferrer
          seeOtherRedirect referrer
        False -> doNotLogIn (Just "Incorrect information.")
                            maybeEmail


doNotLogIn :: Maybe String -> Maybe String -> FruitTart CGIResult
doNotLogIn maybeWarning maybeEmail = do
  bind "Templates" "pageTitle" "Log In"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getNavigationBar "/login/login/"
  bind "Templates" "navigationBar" navigationBar
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  unbind "Templates" "loginButton"
  referrer <- getReferrer
  bind "Base.Controller.Login" "referrer" referrer
  bind "Base.Controller.Login" "maybeWarning" maybeWarning
  bind "Base.Controller.Login" "maybeEmail" maybeEmail
  pageContent <- getTemplate "Base.Controller.Login" "login"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


logout :: FruitTart CGIResult
logout = do
  sessionID <- getSessionID
  namedQuery "Base.Controller.Login" "logOut" [SQLInteger sessionID]
  referrer <- getReferrer
  seeOtherRedirect referrer


accountGET :: FruitTart CGIResult
accountGET = do
  outputAccountPage


accountPOST :: FruitTart CGIResult
accountPOST = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
      defaultPage <- getDefaultPage
      seeOtherRedirect defaultPage
    Just userID -> do
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
      maybeURL <- getInput "url"
      url <- return $ case maybeURL of
                        Just url -> fromCRLF url
                        Nothing -> ""
      namedQuery "Base.Controller.Login" "accountUpdate"
                 [SQLText fullName, SQLText email, SQLText url, SQLInteger userID]
      setPopupMessage $ Just "Edited details."
      outputAccountPage


outputAccountPage :: FruitTart CGIResult
outputAccountPage = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
      defaultPage <- getDefaultPage
      seeOtherRedirect defaultPage
    Just userID -> do
      sessionID <- getSessionID
      bindNamedQuery "Base.Controller.Login" "account" [SQLInteger sessionID]
      bind "Templates" "pageTitle" "Buglist Account"
      pageHeadItems <- getPageHeadItems
      bind "Templates" "pageHeadItems" pageHeadItems
      currentPage <- return "/login/account/"
      navigationBar <- getNavigationBar currentPage
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bind "Base.Controller.Login" "privacyNote" privacyNote
      pageContent <- getTemplate "Base.Controller.Login" "account"
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page"
      output page


passwordGET :: FruitTart CGIResult
passwordGET = do
  seeOtherRedirect "/login/account/"


passwordPOST :: FruitTart CGIResult
passwordPOST = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> seeOtherRedirect "/login/account/"
    Just userID -> do
      maybeOldPassword <- getInput "old-password"
      oldPassword <- return $ case maybeOldPassword of
                      Just oldPassword -> oldPassword
                      Nothing -> ""
      maybeNewPassword1 <- getInput "new-password-1"
      newPassword1 <- return $ case maybeNewPassword1 of
                      Just newPassword1 -> newPassword1
                      Nothing -> ""
      maybeNewPassword2 <- getInput "new-password-2"
      newPassword2 <- return $ case maybeNewPassword2 of
                      Just newPassword2 -> newPassword2
                      Nothing -> ""
      valid <- validatePassword userID oldPassword
      performChange <- return $ valid
                                && (newPassword1 == newPassword2)
                                && (newPassword1 /= "")
      if performChange
         then do
           namedQuery "Base.Controller.Login" "accountUpdatePassword"
                      [SQLBlob $ hashPassword newPassword1,
                       SQLInteger userID]
           setPopupMessage $ Just "Password changed."
           seeOtherRedirect "/login/account/"
         else do
           setPopupMessage $ Just "Password NOT changed."
           seeOtherRedirect "/login/account/"


getReferrer :: FruitTart String
getReferrer = do
  maybeReferrer <- getInput "referrer"
  case maybeReferrer of
    Just referrer -> return referrer
    Nothing -> do
                maybeHTTPReferrer <- requestHeader "Referer"
                case maybeHTTPReferrer of
                  Just referrer -> return referrer
                  Nothing -> getDefaultPage

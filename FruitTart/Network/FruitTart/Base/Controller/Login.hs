module Network.FruitTart.Base.Controller.Login (actionTable)
    where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map, head)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Int
import Data.List

import Network.FruitTart.Base
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates


actionTable :: ActionTable
actionTable
    = makeActionTable [("login", "GET", [], [], [], toDyn loginGET),
                       ("login", "POST", [], [], [], toDyn loginPOST),
                       ("logout", "GET", [], [], [], toDyn logout),
                       ("account", "GET", [], [], [], toDyn accountGET),
                       ("account", "POST", [], [], [], toDyn accountPOST),
                       ("password", "GET", [], [], [], toDyn passwordGET),
                       ("password", "POST", [], [], [], toDyn passwordPOST)]


loginGET :: FruitTart ()
loginGET = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
             sessionID <- getSessionID
             queryResult
                 <- namedQuery "Base.Login" "recentUser"
                               [SQLInteger sessionID]
             maybeRecentUserEmail
                 <- return $ fromJust
                           $ Map.lookup ("Base.Login", "maybeEmail")
                                        $ head queryResult
             maybeRecentUserEmail
                 <- return $ case maybeRecentUserEmail of
                               TemplateMaybe Nothing -> Nothing
                               TemplateMaybe (Just (TemplateString string)) -> Just string
             doNotLogIn Nothing maybeRecentUserEmail
    Just _ -> do
             targetPage <- getReferrer
             seeOtherRedirect targetPage


loginPOST :: FruitTart ()
loginPOST = do
  maybeEmail <- getInput "email"
  email <- return $ case maybeEmail of
                      Just email -> email
                      Nothing -> ""
  maybePassword <- getInput "password"
  password <- return $ case maybePassword of
                         Just password -> password
                         Nothing -> ""
  maybeUserIDRows <- namedQuery "Base.Login" "userByEmail" [SQLText email]
  case maybeUserIDRows of
    [] -> doNotLogIn (Just "Incorrect information.")
                     maybeEmail
    [values] -> do
      userID <- return $ fromJust $ Map.lookup ("Base.Login", "userID")
                                               values
      userID <- return $ case userID of
                           TemplateInteger integer -> integer
      valid <- validatePassword userID password
      case valid of
        True -> do
          sessionID <- getSessionID
          namedQuery "Base.Login" "logIn"
                     [SQLInteger userID, SQLInteger userID, SQLInteger sessionID]
          referrer <- getReferrer
          seeOtherRedirect referrer
        False -> doNotLogIn (Just "Incorrect information.")
                            maybeEmail


doNotLogIn :: Maybe String -> Maybe String -> FruitTart ()
doNotLogIn maybeWarning maybeEmail = do
  bind "Base" "pageTitle" "Log In"
  pageHeadItems <- getTemplate "Base" "pageHeadItems" [TemplateString "Base.Login"]
  bind "Base" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString "/login/login/"]
  bind "Base" "navigationBar" navigationBar
  popupMessage <- getPopupMessage
  bind "Base" "popupMessage" popupMessage
  unbind "Base" "loginButton"
  referrer <- getReferrer
  bind "Base.Login" "referrer" referrer
  bind "Base" "maybeWarning" maybeWarning
  bind "Base.Login" "maybeEmail" maybeEmail
  pageContent <- getTemplate "Base.Login" "login" []
  bind "Base" "pageContent" pageContent
  page <- getTemplate "Base" "page" []
  fPutStr page


logout :: FruitTart ()
logout = do
  sessionID <- getSessionID
  namedQuery "Base.Login" "logOut" [SQLInteger sessionID]
  referrer <- getReferrer
  seeOtherRedirect referrer


accountGET :: FruitTart ()
accountGET = do
  outputAccountPage


accountPOST :: FruitTart ()
accountPOST = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
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
      namedQuery "Base.Login" "accountUpdate"
                 [SQLText fullName, SQLText email, SQLText url, SQLInteger userID]
      setPopupMessage $ Just "Edited details."
      outputAccountPage


outputAccountPage :: FruitTart ()
outputAccountPage = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
      defaultPage <- getTemplate "Base" "defaultPage" []
      seeOtherRedirect defaultPage
    Just userID -> do
      sessionID <- getSessionID
      bindNamedQuery "Base.Login" "account" [SQLInteger sessionID]
      bind "Base" "pageTitle" "Buglist Account"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Base.Login"]
      bind "Base" "pageHeadItems" pageHeadItems
      currentPage <- return "/login/account/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bind "Base.Login" "privacyNote" privacyNote
      pageContent <- getTemplate "Base.Login" "account" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
      fPutStr page


passwordGET :: FruitTart ()
passwordGET = do
  seeOtherRedirect "/login/account/"


passwordPOST :: FruitTart ()
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
           namedQuery "Base.Login" "accountUpdatePassword"
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
                maybeHTTPReferrer <- getRequestHeader HttpReferer
                case maybeHTTPReferrer of
                  Just referrer -> return referrer
                  Nothing -> getTemplate "Base" "defaultPage" []

module Network.FruitTart.Controller.Login (actionTable,
                                           getLoggedInUser,
                                           getEffectiveUser,
                                           outputMustLoginPage)
    where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List
import Network.FastCGI hiding (output)

import Network.FruitTart.Controller.PopupMessage
import Network.FruitTart.Dispatcher
import Network.FruitTart.Util
import Network.FruitTart.View.Misc
import Network.FruitTart.View.Navigation


actionTable :: ActionTable
actionTable
    = Map.fromList [("login",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn loginGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn loginPOST))]),
                      ("logout",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn logout))]),
                      ("account",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn accountGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn accountPOST))]),
                      ("password",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn passwordGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn passwordPOST))])]


outputMustLoginPage :: String -> FruitTart CGIResult
outputMustLoginPage currentPage = do
  pageHeadItems <- getPageHeadItems
  navigationBar <- getNavigationBar currentPage
  output $ "<html><head>\n"
         ++ "<title>Buglist Users</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ "<h1>You must log in to access this page.</h1>\n"
         ++ "</body></html>"


loginGET :: FruitTart CGIResult
loginGET = do
  sessionID <- getSessionID
  [[maybeRecentUserEmail]]
      <- query ("SELECT users.email FROM sessions LEFT JOIN users "
                ++ "ON sessions.recent_user = users.id WHERE sessions.id = ?")
               [SQLInteger sessionID]
  maybeRecentUserEmail <- return $ case maybeRecentUserEmail of
                                     SQLNull -> Nothing
                                     SQLText email -> Just email
  doNotLogIn Nothing maybeRecentUserEmail


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
  maybeUserIDRows <- query "SELECT id FROM users WHERE email = ?" [SQLText email]
  case maybeUserIDRows of
    [] -> doNotLogIn (Just "Incorrect information.")
                     maybeEmail
    [[SQLInteger userID]] -> do
      valid <- validatePassword userID password
      case valid of
        True -> do
          sessionID <- getSessionID
          query "UPDATE sessions SET recent_user = ?, logged_in_user = ? WHERE id = ?"
                [SQLInteger userID, SQLInteger userID, SQLInteger sessionID]
          referrer <- getReferrer
          seeOtherRedirect referrer
        False -> doNotLogIn (Just "Incorrect information.")
                            maybeEmail


doNotLogIn :: Maybe String -> Maybe String -> FruitTart CGIResult
doNotLogIn maybeWarning maybeEmail = do
  referrer <- getReferrer
  pageHeadItems <- getPageHeadItems
  navigationBar <- getNavigationBar "/login/login/"
  output $ "<html><head>\n"
         ++ "<title>Buglist Users</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ "<div class=\"form mini\">\n"
         ++ "<h2>Log In</h2>\n"
         ++ "<form method=\"POST\" action=\"/login/login/\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ (escapeHTML warning)
                              ++ "</div>\n"
              Nothing -> ""
         ++ "<div><b>Email:</b> "
         ++ "<input type=\"text\" size=\"15\" name=\"email\" value=\""
         ++ (case maybeEmail of
               Just email -> (escapeAttribute email)
               Nothing -> "")
         ++ "\"/></div>\n"
         ++ "<div><b>Password:</b> "
         ++ "<input type=\"password\" size=\"10\" name=\"password\" value=\""
         ++ "\"/></div>\n"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Log In\">Log In</button>"
         ++ "</div>\n"
         ++ "<input type=\"hidden\" name=\"referrer\" value=\""
         ++ (escapeAttribute referrer)
         ++ "\"/>"
         ++ "</form>\n"
         ++ "</body></html>"


logout :: FruitTart CGIResult
logout = do
  sessionID <- getSessionID
  query "UPDATE sessions SET logged_in_user = NULL WHERE id = ?"
        [SQLInteger sessionID]
  referrer <- getReferrer
  seeOtherRedirect referrer


accountGET :: FruitTart CGIResult
accountGET = do
  outputAccountPage


accountPOST :: FruitTart CGIResult
accountPOST = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Nothing -> seeOtherRedirect "/issues/index/"
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
      query "UPDATE users SET full_name = ?, email = ? WHERE id = ?"
            [SQLText fullName, SQLText email, SQLInteger userID]
      setPopupMessage $ Just "Edited details."
      outputAccountPage


outputAccountPage :: FruitTart CGIResult
outputAccountPage = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Nothing -> seeOtherRedirect "/issues/index/"
    Just userID -> do
      sessionID <- getSessionID
      [[SQLText fullName, SQLText email]]
          <- query (  "SELECT users.full_name, users.email "
                   ++ "FROM sessions LEFT JOIN users "
                   ++ "ON sessions.logged_in_user = users.id "
                   ++ "WHERE sessions. id = ?")
                   [SQLInteger sessionID]
      pageHeadItems <- getPageHeadItems
      currentPage <- return "/login/account/"
      navigationBar <- getNavigationBar currentPage
      loginButton <- getLoginButton currentPage
      popupMessage <- getPopupMessage
      output $ "<html><head>\n"
         ++ "<title>Buglist Account</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<h1>Account</h1>"
         ++ "<div class=\"form\">\n"
         ++ "<h2>Edit Account Details</h2>\n"
         ++ "<form method=\"POST\" action=\"/login/account/\">\n"
         ++ "<div><b>Full Name:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"full-name\" value=\""
         ++ (escapeAttribute fullName)
         ++ "\"/></div>\n"
         ++ "<div><b>Email:</b> "
         ++ "<input type=\"text\" size=\"30\" name=\"email\" value=\""
         ++ (escapeAttribute email)
         ++ "\"/>"
         ++ "<br />" ++ (escapeHTML privacyNote)
         ++ "</div>\n"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Save\">Save</button>"
         ++ "</div>\n"
         ++ "</form>\n"
         ++ "</div>\n"
         ++ "<div class=\"form\">\n"
         ++ "<h2>Change Password</h2>\n"
         ++ "<form method=\"POST\" action=\"/login/password/\">\n"
         ++ "<table class=\"layout\">\n"
         ++ "<tr>\n"
         ++ "<td><b>Old Password:</b></td>\n"
         ++ "<td><input type=\"password\" size=\"10\" name=\"old-password\" value=\""
         ++ "\"/></td>\n"
         ++ "</tr>\n"
         ++ "<tr>\n"
         ++ "<td><b>New Password:</b></td>\n"
         ++ "<td><input type=\"password\" size=\"10\" name=\"new-password-1\" value=\""
         ++ "\"/></td>\n"
         ++ "</tr>\n"
         ++ "<tr>\n"
         ++ "<td><b>New Password Again:</b></td>\n"
         ++ "<td><input type=\"password\" size=\"10\" name=\"new-password-2\" value=\""
         ++ "\"/></td>\n"
         ++ "</tr>\n"
         ++ "</table>\n"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Change\">Change</button>"
         ++ "</div>\n"
         ++ "</form>\n"
         ++ "</div>\n"


passwordGET :: FruitTart CGIResult
passwordGET = do
  seeOtherRedirect "/login/account/"


passwordPOST :: FruitTart CGIResult
passwordPOST = do
  maybeUserID <- getLoggedInUser
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
           query "UPDATE users SET password_hash = ? WHERE id = ?"
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
                  Nothing -> return ""


getLoggedInUser :: FruitTart (Maybe Int64)
getLoggedInUser = do
  sessionID <- getSessionID
  [[maybeUserID]] <- query "SELECT logged_in_user FROM sessions WHERE id = ?"
                           [SQLInteger sessionID]
  return $ case maybeUserID of
             SQLNull -> Nothing
             SQLInteger userID -> Just userID


getEffectiveUser :: FruitTart Int64
getEffectiveUser = do
  maybeUserID <- getLoggedInUser
  case maybeUserID of
    Just userID -> return userID
    Nothing -> do
      [[SQLInteger anonymousID]] <- query "SELECT anonymous_user FROM settings LIMIT 1"
                                          []
      return anonymousID

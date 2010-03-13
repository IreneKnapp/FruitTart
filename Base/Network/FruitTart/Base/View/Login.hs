module Network.FruitTart.Base.View.Login (
                                          getLoginButton,
                                          outputMustLoginPage,
                                          getLoggedInUserID,
                                          getEffectiveUserID,
                                          getOrCreateUserID,
                                          getCanActAsUser,
                                          getRightAdminDesign,
                                          defaultFullName,
                                          defaultEmail,
                                          privacyNote
                                         )
    where

import Network.FruitTart.Base
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Base.View.Navigation


getLoginButton :: String -> FruitTart String
getLoginButton currentPage = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Nothing -> do
      sessionID <- getSessionID
      [[maybeEmail]] <- query ("SELECT users.email FROM sessions LEFT JOIN users "
                               ++ "ON users.id = sessions.recent_user "
                               ++ "WHERE sessions.id = ?")
                              [SQLInteger sessionID]
      email <- return $ case maybeEmail of
                          SQLNull -> ""
                          SQLText email -> email
      return ("<div id=\"navigationright\">"
              ++ "<a id=\"loginlink\" class=\"login\" href=\"/login/login/\">Log In</a>"
              ++ "</div>\n"
              ++ "<div id=\"loginbox\" style=\"display: none;\">"
              ++ "<form method=\"POST\" action=\"/login/login/\">\n"
              ++ "<div><b>Email:</b> "
              ++ "<input type=\"text\" size=\"15\" name=\"email\" value=\""
              ++ (escapeAttribute email)
              ++ "\"/></div>\n"
              ++ "<div><b>Password:</b> "
              ++ "<input type=\"password\" size=\"10\" name=\"password\" value=\""
              ++ "\"/></div>\n"
              ++ "<div class=\"submit\">"
              ++ "<button type=\"submit\" value=\"Log In\">Log In</button>"
              ++ "</div>\n"
              ++ "</form>\n"
              ++ "</div>\n")
    Just _ -> do
      return ("<div id=\"navigationright\">"
              ++ (if currentPage == "/login/account/"
                     then "<b>Account</b>"
                     else "<a href=\"/login/account/\">Account</a>")
              ++ "<a id=\"loginlink\" class=\"logout\" href=\"/login/logout/\">"
              ++ "Log Out</a></div>\n")


outputMustLoginPage :: String -> FruitTart CGIResult
outputMustLoginPage currentPage = do
  pageHeadItems <- getPageHeadItems
  navigationBar <- getNavigationBar currentPage
  output $ "<html><head>\n"
         ++ "<title>Login Required</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ "<h1>You must log in to access this page.</h1>\n"
         ++ "</body></html>"


getLoggedInUserID :: FruitTart (Maybe Int64)
getLoggedInUserID = do
  sessionID <- getSessionID
  [[maybeUserID]] <- query "SELECT logged_in_user FROM sessions WHERE id = ?"
                           [SQLInteger sessionID]
  return $ case maybeUserID of
             SQLNull -> Nothing
             SQLInteger userID -> Just userID


getEffectiveUserID :: FruitTart Int64
getEffectiveUserID = do
  maybeUserID <- getLoggedInUserID
  case maybeUserID of
    Just userID -> return userID
    Nothing -> do
      [[SQLInteger anonymousID]] <- query "SELECT anonymous_user FROM settings LIMIT 1"
                                          []
      return anonymousID


getOrCreateUserID :: String -> String -> String -> FruitTart Int64
getOrCreateUserID fullName email url = do
  if (email == "") || (email == "anonymous")
     then do
       rows <- query "SELECT id FROM users WHERE full_name == ? AND email == 'anonymous'"
                     [SQLText fullName]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, url, password_hash) "
                   ++ "VALUES (?, 'anonymous', ?, NULL)")
                  [SQLText fullName, SQLText url]
            [[SQLInteger id]]
                <- query ("SELECT id FROM users WHERE full_name == ? "
                          ++ "AND email == 'anonymous'")
                         [SQLText fullName]
            return id
     else do
       rows <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, url, password_hash) "
                   ++ "VALUES (?, ?, ?, NULL)")
                  [SQLText fullName, SQLText email, SQLText url]
            [[SQLInteger id]]
                <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
            return id


getCanActAsUser :: Int64 -> FruitTart Bool
getCanActAsUser userID = do
  effectiveUserID <- getEffectiveUserID
  if effectiveUserID == userID
     then return True
     else do
       [[SQLInteger isNull]]
           <- query "SELECT password_hash IS NULL FROM users WHERE id = ?"
                    [SQLInteger userID]
       return $ case isNull of
                  0 -> False
                  _ -> True


getRightAdminDesign :: FruitTart Bool
getRightAdminDesign = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_admin_design FROM base_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True


defaultFullName :: String
defaultFullName = "Anonymous"


defaultEmail :: String
defaultEmail = "anonymous"


privacyNote :: String
privacyNote
    = "Your email will be visible only to members who have accounts."

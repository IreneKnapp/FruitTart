module Network.FruitTart.View.Login (getLoginButton) where

import Network.FruitTart.Base
import {-# SOURCE #-} Network.FruitTart.Controller.Login
import Network.FruitTart.Util


getLoginButton :: String -> FruitTart String
getLoginButton currentPage = do
  maybeUserID <- getLoggedInUser
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

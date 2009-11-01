module Controller.Login where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List
import Network.FastCGI hiding (output)

import Buglist
import Database
import {-# SOURCE #-} Dispatcher
import HTML
import Lists
import Passwords
import SQLite3 (SQLData(..))
import Types


loginGET :: Buglist CGIResult
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


loginPOST :: Buglist CGIResult
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


doNotLogIn :: Maybe String -> Maybe String -> Buglist CGIResult
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


logout :: Buglist CGIResult
logout = do
  sessionID <- getSessionID
  query "UPDATE sessions SET logged_in_user = NULL WHERE id = ?"
        [SQLInteger sessionID]
  referrer <- getReferrer
  seeOtherRedirect referrer


getReferrer :: Buglist String
getReferrer = do
  maybeReferrer <- getInput "referrer"
  case maybeReferrer of
    Just referrer -> return referrer
    Nothing -> do
                maybeHTTPReferrer <- requestHeader "Referer"
                case maybeHTTPReferrer of
                  Just referrer -> return referrer
                  Nothing -> return ""

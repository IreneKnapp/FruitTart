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
import SQLite3 (SQLData(..))
import Types


loginGET :: Buglist CGIResult
loginGET = do
  pageHeadItems <- getPageHeadItems
  navigationBar <- getNavigationBar "/login/login/"
  output  $ "<html><head>\n"
         ++ "<title>Buglist Users</title>\n"
         ++ pageHeadItems
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
          ++ "<div class=\"form mini\">\n"
          ++ "<h2>Log In</h2>\n"
          ++ "<form method=\"POST\" action=\"/login/login/\">\n"
          ++ "<div><b>Email:</b> "
          ++ "<input type=\"text\" size=\"15\" name=\"email\" value=\""
          ++ (escapeAttribute "")
          ++ "\"/></div>\n"
          ++ "<div><b>Password:</b> "
          ++ "<input type=\"password\" size=\"10\" name=\"email\" value=\""
          ++ (escapeAttribute "")
          ++ "\"/></div>\n"
          ++ "<div class=\"submit\">"
          ++ "<button type=\"submit\" value=\"Log In\">Log In</button>"
          ++ "</div>\n"
          ++ "</form>\n"
         ++ "</body></html>"


loginPOST :: Buglist CGIResult
loginPOST = do
  output "Not implemented."


logout :: Buglist CGIResult
logout = do
  output "Not implemented."

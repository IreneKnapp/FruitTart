module Network.FruitTart.Buglist.Controller.Users (
                                                   actionTable
                                                  )
    where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import Network.FruitTart.Base
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Buglist.View.Navigation


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view)]


index :: FruitTart ()
index = do
  rightViewModulesRequiringLogin <- getRightViewModulesRequiringLogin
  case rightViewModulesRequiringLogin of
    False -> outputMustLoginPage "/users/index/"
    True -> do
      bind "Templates" "pageTitle" "Buglist Users"
      pageHeadItems <- getPageHeadItems
      bind "Templates" "pageHeadItems" pageHeadItems
      currentPage <- return "/users/index/"
      navigationBar <- getNavigationBar currentPage
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Buglist.Controller.Users" "indexRows" []
      pageContent <- getTemplate "Buglist.Controller.Users" "index"
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page"
      fPutStr page


view :: Int64 -> FruitTart ()
view id = do
  rightViewModulesRequiringLogin <- getRightViewModulesRequiringLogin
  case rightViewModulesRequiringLogin of
    False -> outputMustLoginPage "/users/index/"
    True -> do
      rows <- query "SELECT full_name, email FROM users WHERE id = ?"
                    [SQLInteger id]
      case rows of
        [[SQLText fullName, SQLText email]]
          -> do
           bind "Templates" "pageTitle" $ escapeHTML fullName
           pageHeadItems <- getPageHeadItems
           bind "Templates" "pageHeadItems" pageHeadItems
           currentPage <- return $ "/users/view/" ++ (show id) ++ "/"
           navigationBar <- getNavigationBar currentPage
           bind "Templates" "navigationBar" navigationBar
           loginButton <- getLoginButton currentPage
           bind "Templates" "loginButton" loginButton
           popupMessage <- getPopupMessage
           bind "Templates" "popupMessage" popupMessage
           bind "Buglist.Controller.Users" "fullName" fullName
           bind "Buglist.Controller.Users" "email" email
           bind "Buglist.Controller.Users" "userID" id
           bindNamedQuery "Buglist.Controller.Users" "userIssueActions" []
           pageContent <- getTemplate "Buglist.Controller.Users" "view"
           bind "Templates" "pageContent" pageContent
           page <- getTemplate "Templates" "page"
           fPutStr page
        _ -> errorInvalidID "user"

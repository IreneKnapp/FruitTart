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
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Buglist.View.Navigation


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], [], toDyn view)]


index :: FruitTart ()
index = do
  rightViewModulesRequiringLogin <- getRightViewModulesRequiringLogin
  case rightViewModulesRequiringLogin of
    False -> outputMustLoginPage "/users/index/"
    True -> do
      bind "Base" "pageTitle" "Buglist Users"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Buglist.Users"]
      bind "Base" "pageHeadItems" pageHeadItems
      currentPage <- return "/users/index/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Buglist.Users" "indexRows" []
      pageContent <- getTemplate "Buglist.Users" "index" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
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
           bind "Base" "pageTitle" $ escapeHTML fullName
           pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                        [TemplateString "Buglist.Users"]
           bind "Base" "pageHeadItems" pageHeadItems
           currentPage <- return $ "/users/view/" ++ (show id) ++ "/"
           navigationBar <- getTemplate "Base" "navigationBar"
                                        [TemplateString currentPage]
           bind "Base" "navigationBar" navigationBar
           loginButton <- getLoginButton currentPage
           bind "Base" "loginButton" loginButton
           popupMessage <- getPopupMessage
           bind "Base" "popupMessage" popupMessage
           bind "Buglist.Users" "fullName" fullName
           bind "Buglist.Users" "email" email
           bind "Buglist.Users" "userID" id
           bindNamedQuery "Buglist.Users" "userIssueActions" []
           pageContent <- getTemplate "Buglist.Users" "view" []
           bind "Base" "pageContent" pageContent
           page <- getTemplate "Base" "page" []
           fPutStr page
        _ -> errorInvalidID "user"

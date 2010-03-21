module Network.FruitTart.Blog.Controller.Blog (
                                               actionTable
                                              )
    where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Network.FruitTart.Base
import Network.FruitTart.Util
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index)]


index :: FruitTart ()
index = do
  bindDefaults "CS Thoughts" "/blog/index/"
  bindNamedQueryMultipleRows "Blog.Controller.Blog" "indexEntries" []
  outputPage "Blog.Controller.Blog" "index"


bindDefaults :: String -> String -> FruitTart ()
bindDefaults pageTitle currentPage = do
  bind "Templates" "pageTitle" pageTitle
  pageHeadItems <- getTemplate "Templates" "pageHeadItems" [TemplateString "Blog.Index"]
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Blog.Controller.Blog" "maybeWarning" (Nothing :: Maybe String)


outputPage :: String -> String -> FruitTart ()
outputPage moduleName templateName = do
  pageContent <- getTemplate moduleName templateName []
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page" []
  fPutStr page


outputPageNoScript :: String -> String -> FruitTart ()
outputPageNoScript moduleName templateName = do
  pageContent <- getTemplate moduleName templateName []
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript" []
  fPutStr page

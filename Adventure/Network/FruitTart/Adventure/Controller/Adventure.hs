module Network.FruitTart.Adventure.Controller.Adventure (
                                                         actionTable,
                                                         getRightEdit
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


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("edit-index", "GET", [], [], toDyn editIndex),
                       ("edit-node", "GET", [IDParameter], [], toDyn editNodeGET),
                       ("edit-variable", "GET", [StringParameter], [],
                        toDyn editVariableGET),
                       ("create-node", "GET", [], [], toDyn createNodeGET),
                       ("create-variable", "GET", [], [], toDyn createVariableGET),
                       ("edit-node", "POST", [IDParameter], [], toDyn editNodePOST),
                       ("edit-variable", "POST", [StringParameter], [],
                        toDyn editVariablePOST),
                       ("create-node", "POST", [], [], toDyn createNodePOST),
                       ("create-variable", "POST", [], [], toDyn createVariablePOST)]


index :: FruitTart CGIResult
index = do
  bindDefaults "Adventure!" "/adventure/index/"
  outputPage "Adventure.Controller.Adventure" "index"


editIndex :: FruitTart CGIResult
editIndex = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" "/adventure/edit-index/"
      bindNamedQueryMultipleRows "Adventure.Controller.Adventure" "nodes" []
      bindNamedQueryMultipleRows "Adventure.Controller.Adventure" "variables" []
      outputPage "Adventure.Controller.Adventure" "editIndex"


editNodeGET :: Int64 -> FruitTart CGIResult
editNodeGET nodeID = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bind "Adventure.Controller.Adventure" "id" nodeID
      bindNamedQuery "Adventure.Controller.Adventure" "nodeDetails" [SQLInteger nodeID]
      name <- getBinding "Adventure.Controller.Adventure" "name" >>= return . fromJust
      name <- return $ case name of
                         TemplateString string -> string
      bindNamedQueryMultipleRows "Adventure.Controller.Adventure" "options"
                                 [SQLInteger nodeID]
      currentPage <- return $ "/adventure/edit-node/" ++ (show nodeID) ++ "/"
      bindDefaults ("Edit Node " ++ name) currentPage
      bind "Adventure.Controller.Adventure" "targetPage" currentPage
      outputPage "Adventure.Controller.Adventure" "editNode"


editVariableGET :: String -> FruitTart CGIResult
editVariableGET variableName = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults ("Edit Variable " ++ variableName)
                   ("/adventure/edit-variable/" ++ variableName ++ "/")
      bindNamedQuery "Adventure.Controller.Adventure" "variableDetails"
                     [SQLText variableName]
      outputPageNoScript "Adventure.Controller.Adventure" "editVariable"


createNodeGET :: FruitTart CGIResult
createNodeGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Node" "/adventure/create-node/"
      bind "Adventure.Controller.Adventure" "name" "New Node"
      bind "Adventure.Controller.Adventure" "body" ""
      bind "Adventure.Controller.Adventure" "options" ([] :: [String])
      bind "Adventure.Controller.Adventure" "targetPage" "/adventure/create-node/"
      outputPage "Adventure.Controller.Adventure" "editNode"


createVariableGET :: FruitTart CGIResult
createVariableGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Variable" "/adventure/create-variable/"
      outputPage "Adventure.Controller.Adventure" "editVariable"


editNodePOST :: Int64 -> FruitTart CGIResult
editNodePOST nodeID = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


editVariablePOST :: String -> FruitTart CGIResult
editVariablePOST variableName = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


createNodePOST :: FruitTart CGIResult
createNodePOST = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


createVariablePOST :: FruitTart CGIResult
createVariablePOST = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


bindDefaults :: String -> String -> FruitTart ()
bindDefaults pageTitle currentPage = do
  bind "Templates" "pageTitle" pageTitle
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems"
       (pageHeadItems
        ++ "<link href=\"/css/adventure.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
        ++ "<script src=\"/js/adventure.js\" type=\"text/ecmascript\"></script>\n")
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage


outputPage :: String -> String -> FruitTart CGIResult
outputPage moduleName templateName = do
  pageContent <- getTemplate moduleName templateName
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


outputPageNoScript :: String -> String -> FruitTart CGIResult
outputPageNoScript moduleName templateName = do
  pageContent <- getTemplate moduleName templateName
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript"
  output page


getRightEdit :: FruitTart Bool
getRightEdit = do
  userID <- getEffectiveUserID
  [values] <- namedQuery "Adventure.Controller.Adventure" "getRights"
                         [SQLInteger userID]
  right <- return $ fromJust $ Map.lookup ("Adventure.Controller.Adventure", "rightEdit")
                                          values
  right <- return $ case right of
                      TemplateBool bool -> bool
  return right

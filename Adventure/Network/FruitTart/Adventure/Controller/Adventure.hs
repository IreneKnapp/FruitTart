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
      bindQueryMultipleRows "Adventure.Controller.Adventure"
                            "nodes"
                            [("id", TInt),
                             ("name", TString)]
                            "SELECT id, name FROM adventure_nodes ORDER BY name"
                            []
      bindQueryMultipleRows "Adventure.Controller.Adventure"
                            "variables"
                            [("name", TString)]
                            "SELECT name FROM adventure_variables ORDER BY name"
                            []
      outputPage "Adventure.Controller.Adventure" "editIndex"


editNodeGET :: Int64 -> FruitTart CGIResult
editNodeGET nodeID = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" ("/adventure/edit-node/" ++ (show nodeID) ++ "/")
      bindQuery "Adventure.Controller.Adventure"
                [("id", TInt),
                 ("name", TString),
                 ("body", TString)]
                "SELECT id, name, body FROM adventure_nodes WHERE id = ?"
                [SQLInteger nodeID]
      bindQueryMultipleRows "Adventure.Controller.Adventure"
                            "options"
                            [("id", TInt),
                             ("name", TString),
                             ("child", TInt),
                             ("variable", TMaybeString),
                             ("effect", TMaybeInt)]
                            ("SELECT options.id, options.name, options.child, "
                            ++ "effects.variable, effects.effect "
                            ++ "FROM adventure_options AS options "
                            ++ "LEFT JOIN adventure_option_variable_effects AS effects "
                            ++ "ON effects.option = options.id "
                            ++ "WHERE options.parent = ?")
                            [SQLInteger nodeID]
      outputPage "Adventure.Controller.Adventure" "editNode"


editVariableGET :: String -> FruitTart CGIResult
editVariableGET variableName = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" ("/adventure/edit-variable/" ++ variableName ++ "/")
      bindQuery "Adventure.Controller.Adventure"
                [("name", TString)]
                "SELECT name FROM adventure_variables WHERE name = ?"
                [SQLText variableName]
      outputPage "Adventure.Controller.Adventure" "editVariable"


createNodeGET :: FruitTart CGIResult
createNodeGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" "/adventure/create-node/"
      outputPage "Adventure.Controller.Adventure" "editNode"


createVariableGET :: FruitTart CGIResult
createVariableGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" "/adventure/create-variable/"
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
  bind "Templates" "pageHeadItems" pageHeadItems
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


getRightEdit :: FruitTart Bool
getRightEdit = do
  userID <- getEffectiveUserID
  [[SQLInteger right]]
      <- query "SELECT right_edit FROM adventure_users WHERE id = ?"
               [SQLInteger userID]
  return $ case right of
             0 -> False
             _ -> True

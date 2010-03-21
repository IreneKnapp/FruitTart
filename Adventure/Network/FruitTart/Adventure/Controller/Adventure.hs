module Network.FruitTart.Adventure.Controller.Adventure (
                                                         actionTable,
                                                         getRightEdit
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


index :: FruitTart ()
index = do
  bindDefaults "Adventure!" "/adventure/index/"
  outputPage "Adventure.Controller.Adventure" "index"


editIndex :: FruitTart ()
editIndex = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" "/adventure/edit-index/"
      bindNamedQueryMultipleRows "Adventure.Controller.Adventure" "nodes" []
      bindNamedQueryMultipleRows "Adventure.Controller.Adventure" "variables" []
      outputPage "Adventure.Controller.Adventure" "editIndex"


editNodeGET :: Int64 -> FruitTart ()
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


editVariableGET :: String -> FruitTart ()
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


createNodeGET :: FruitTart ()
createNodeGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Node" "/adventure/create-node/"
      bind "Adventure.Controller.Adventure" "name" "New Node"
      bind "Adventure.Controller.Adventure" "body" ""
      bind "Adventure.Controller.Adventure" "options" ([] :: [Map (String, String)
                                                                  TemplateValue])
      bind "Adventure.Controller.Adventure" "targetPage" "/adventure/create-node/"
      outputPage "Adventure.Controller.Adventure" "editNode"


createVariableGET :: FruitTart ()
createVariableGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Variable" "/adventure/create-variable/"
      outputPage "Adventure.Controller.Adventure" "editVariable"


editNodePOST :: Int64 -> FruitTart ()
editNodePOST nodeID = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      name <- getInput "name"
      name <- return $ case name of
                         Nothing -> ""
                         Just name -> name
      body <- getInput "body"
      body <- return $ case body of
                         Nothing -> ""
                         Just body -> fromCRLF body
      options <- getOptionInputs
      [values] <- namedQuery "Adventure.Controller.Adventure" "nodeExistsWithDifferentID"
                             [SQLText name, SQLInteger nodeID]
      exists <- return $ fromJust
                       $ Map.lookup ("Adventure.Controller.Adventure", "exists")
                                    values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      if exists
         then do
           bindDefaults "Create Node" "/adventure/create-node/"
           bind "Adventure.Controller.Adventure" "name" name
           bind "Adventure.Controller.Adventure" "body" body
           bind "Adventure.Controller.Adventure" "options" options
           bind "Adventure.Controller.Adventure" "targetPage" "/adventure/create-node/"
           bind "Adventure.Controller.Adventure" "maybeWarning"
                $ Just "There is already a node by that name."
           outputPage "Adventure.Controller.Adventure" "editNode"
         else do
           namedQuery "Adventure.Controller.Adventure" "updateNode"
                      [SQLText name, SQLText body, SQLInteger nodeID]
           setPopupMessage $ Just "Node modified."
           seeOtherRedirect $ "/adventure/edit-node/" ++ (show nodeID) ++ "/"


editVariablePOST :: String -> FruitTart ()
editVariablePOST variableName = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


createNodePOST :: FruitTart ()
createNodePOST = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      name <- getInput "name"
      name <- return $ case name of
                         Nothing -> ""
                         Just name -> name
      body <- getInput "body"
      body <- return $ case body of
                         Nothing -> ""
                         Just body -> fromCRLF body
      options <- getOptionInputs
      [values] <- namedQuery "Adventure.Controller.Adventure" "nodeExists"
                             [SQLText name]
      exists <- return $ fromJust
                       $ Map.lookup ("Adventure.Controller.Adventure", "exists")
                                    values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      if exists
         then do
           bindDefaults "Create Node" "/adventure/create-node/"
           bind "Adventure.Controller.Adventure" "name" name
           bind "Adventure.Controller.Adventure" "body" body
           bind "Adventure.Controller.Adventure" "options" options
           bind "Adventure.Controller.Adventure" "targetPage" "/adventure/create-node/"
           bind "Adventure.Controller.Adventure" "maybeWarning"
                $ Just "There is already a node by that name."
           outputPage "Adventure.Controller.Adventure" "editNode"
         else do
           namedQuery "Adventure.Controller.Adventure" "insertNode"
                      [SQLText name, SQLText body]
           [values] <- namedQuery "Adventure.Controller.Adventure" "nodeJustInserted" []
           nodeID <- return $ fromJust $ Map.lookup ("Adventure.Controller.Adventure",
                                                     "nodeID")
                                                    values
           nodeID <- return $ case nodeID of
                                TemplateInteger integer -> integer
           setPopupMessage $ Just "Node created."
           seeOtherRedirect $ "/adventure/edit-node/" ++ (show nodeID) ++ "/"


createVariablePOST :: FruitTart ()
createVariablePOST = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      seeOtherRedirect "/adventure/edit-index/"


bindDefaults :: String -> String -> FruitTart ()
bindDefaults pageTitle currentPage = do
  bind "Templates" "pageTitle" pageTitle
  pageHeadItems <- getTemplate "Templates" "pageHeadItems" [TemplateString "Adventure"]
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Adventure.Controller.Adventure" "maybeWarning" (Nothing :: Maybe String)


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


getOptionInputs :: FruitTart [Map (String, String) TemplateValue]
getOptionInputs = return []

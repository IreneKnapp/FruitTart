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
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], [], toDyn index),
                       ("edit-index", "GET", [], [], [], toDyn editIndex),
                       ("edit-node", "GET", [IDParameter], [], [], toDyn editNodeGET),
                       ("edit-variable", "GET", [StringParameter], [], [],
                        toDyn editVariableGET),
                       ("create-node", "GET", [], [], [], toDyn createNodeGET),
                       ("create-variable", "GET", [], [], [], toDyn createVariableGET),
                       ("edit-node", "POST", [IDParameter], [], [], toDyn editNodePOST),
                       ("edit-variable", "POST", [StringParameter], [], [],
                        toDyn editVariablePOST),
                       ("create-node", "POST", [], [], [], toDyn createNodePOST),
                       ("create-variable", "POST", [], [], [], toDyn createVariablePOST)]


index :: FruitTart ()
index = do
  bindDefaults "Adventure!" "/adventure/index/"
  outputPage "Adventure" "index"


editIndex :: FruitTart ()
editIndex = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Adventure Editor" "/adventure/edit-index/"
      bindNamedQueryMultipleRows "Adventure" "nodes" []
      bindNamedQueryMultipleRows "Adventure" "variables" []
      outputPage "Adventure" "editIndex"


editNodeGET :: Int64 -> FruitTart ()
editNodeGET nodeID = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bind "Adventure" "id" nodeID
      bindNamedQuery "Adventure" "nodeDetails" [SQLInteger nodeID]
      name <- getBinding "Adventure" "name" >>= return . fromJust
      name <- return $ case name of
                         TemplateString string -> string
      bindNamedQueryMultipleRows "Adventure" "options"
                                 [SQLInteger nodeID]
      currentPage <- return $ "/adventure/edit-node/" ++ (show nodeID) ++ "/"
      bindDefaults ("Edit Node " ++ name) currentPage
      bind "Adventure" "targetPage" currentPage
      outputPage "Adventure" "editNode"


editVariableGET :: String -> FruitTart ()
editVariableGET variableName = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults ("Edit Variable " ++ variableName)
                   ("/adventure/edit-variable/" ++ variableName ++ "/")
      bindNamedQuery "Adventure" "variableDetails"
                     [SQLText variableName]
      outputPageNoScript "Adventure" "editVariable"


createNodeGET :: FruitTart ()
createNodeGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Node" "/adventure/create-node/"
      bind "Adventure" "name" "New Node"
      bind "Adventure" "body" ""
      bind "Adventure" "options" ([] :: [Map (String, String) TemplateValue])
      bind "Adventure" "targetPage" "/adventure/create-node/"
      outputPage "Adventure" "editNode"


createVariableGET :: FruitTart ()
createVariableGET = do
  right <- getRightEdit
  case right of
    False -> seeOtherRedirect "/adventure/index/"
    True -> do
      bindDefaults "Create Variable" "/adventure/create-variable/"
      outputPage "Adventure" "editVariable"


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
      [values] <- namedQuery "Adventure" "nodeExistsWithDifferentID"
                             [SQLText name, SQLInteger nodeID]
      exists <- return $ fromJust
                       $ Map.lookup ("Adventure", "exists")
                                    values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      if exists
         then do
           bindDefaults "Create Node" "/adventure/create-node/"
           bind "Adventure" "name" name
           bind "Adventure" "body" body
           bind "Adventure" "options" options
           bind "Adventure" "targetPage" "/adventure/create-node/"
           bind "Base" "maybeWarning"
                $ Just "There is already a node by that name."
           outputPage "Adventure" "editNode"
         else do
           namedQuery "Adventure" "updateNode"
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
      [values] <- namedQuery "Adventure" "nodeExists"
                             [SQLText name]
      exists <- return $ fromJust
                       $ Map.lookup ("Adventure", "exists")
                                    values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      if exists
         then do
           bindDefaults "Create Node" "/adventure/create-node/"
           bind "Adventure" "name" name
           bind "Adventure" "body" body
           bind "Adventure" "options" options
           bind "Adventure" "targetPage" "/adventure/create-node/"
           bind "Base" "maybeWarning"
                $ Just "There is already a node by that name."
           outputPage "Adventure" "editNode"
         else do
           namedQuery "Adventure" "insertNode"
                      [SQLText name, SQLText body]
           [values] <- namedQuery "Adventure" "nodeJustInserted" []
           nodeID <- return $ fromJust $ Map.lookup ("Adventure",
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
  bind "Base" "pageTitle" pageTitle
  pageHeadItems <- getTemplate "Base" "pageHeadItems" [TemplateString "Adventure"]
  bind "Base" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
  bind "Base" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Base" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Base" "popupMessage" popupMessage
  bind "Base" "maybeWarning" (Nothing :: Maybe String)


outputPage :: String -> String -> FruitTart ()
outputPage moduleName templateName = do
  pageContent <- getTemplate moduleName templateName []
  bind "Base" "pageContent" pageContent
  page <- getTemplate "Base" "page" []
  fPutStr page


outputPageNoScript :: String -> String -> FruitTart ()
outputPageNoScript moduleName templateName = do
  pageContent <- getTemplate moduleName templateName []
  bind "Base" "pageContent" pageContent
  page <- getTemplate "Base" "pageNoScript" []
  fPutStr page


getRightEdit :: FruitTart Bool
getRightEdit = do
  userID <- getEffectiveUserID
  [values] <- namedQuery "Adventure" "getRights"
                         [SQLInteger userID]
  right <- return $ fromJust $ Map.lookup ("Adventure", "rightEdit")
                                          values
  right <- return $ case right of
                      TemplateBool bool -> bool
  return right


getOptionInputs :: FruitTart [Map (String, String) TemplateValue]
getOptionInputs = return []

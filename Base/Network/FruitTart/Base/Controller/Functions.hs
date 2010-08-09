module Network.FruitTart.Base.Controller.Functions (
                                                    actionTable
                                                   )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

import Network.FruitTart.Base
import Network.FruitTart.Base.Templates.Semantics
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Base.View.Login
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], [], toDyn view),
                       ("create", "GET", [], [], [], toDyn createGET),
                       ("create", "POST", [], [], [], toDyn createPOST),
                       ("edit", "POST", [IDParameter], [], [], toDyn edit),
                       ("copy", "GET", [IDParameter], [], [], toDyn copy),
                       ("delete", "GET", [IDParameter], [], [], toDyn deleteGET),
                       ("delete", "POST", [IDParameter], [], [], toDyn deletePOST)]


index :: FruitTart ()
index = do
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage "/functions/index/"
    True -> do
      bind "Templates" "pageTitle" "All Functions"
      pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                                   [TemplateString "Base.Functions"]
      bind "Templates" "pageHeadItems" pageHeadItems
      currentPage <- return $ "/functions/index/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Base.Controller.Functions"
                                 "indexRows"
                                 []
      pageContent <- getTemplate "Base.Controller.Functions" "index" []
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page" []
      fPutStr page


view :: Int64 -> FruitTart ()
view functionID = do
  let currentPage = "/functions/view/" ++ (show functionID) ++ "/"
      targetPage = "/functions/edit/" ++ (show functionID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeNames
          <- namedQuery "Base.Controller.Functions" "functionDetails"
                        [SQLInteger functionID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          functionName <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                          "functionName")
                                                         values
          functionName <- return $ case functionName of
                                  TemplateString string -> string
          body <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Controller.Functions" "moduleName" moduleName
          bind "Base.Controller.Functions" "functionName" functionName
          bind "Base.Controller.Functions" "body" body
          bindNamedQueryMultipleRows "Base.Controller.Functions"
                                     "parameters"
                                     [SQLInteger functionID]
          outputFunctionPage currentPage targetPage Nothing (Just functionID)
        [] -> errorInvalidID "function"


copy :: Int64 -> FruitTart ()
copy functionID = do
  let currentPage = "/functions/copy/" ++ (show functionID) ++ "/"
      targetPage = "/functions/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeNames
          <- namedQuery "Base.Controller.Functions" "functionDetails"
                        [SQLInteger functionID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          functionName <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                          "functionName")
                                                         values
          functionName <- return $ case functionName of
                                  TemplateString string -> string
          body <- return $ fromJust $ Map.lookup ("Base.Controller.Functions",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Controller.Functions" "moduleName" moduleName
          bind "Base.Controller.Functions" "functionName" functionName
          bind "Base.Controller.Function" "body" body
          bindNamedQueryMultipleRows "Base.Controller.Function"
                                     "parameters"
                                     [SQLInteger functionID]
          outputFunctionPage currentPage targetPage Nothing Nothing
        [] -> errorInvalidID "function"


createGET :: FruitTart ()
createGET = do
  let currentPage = "/functions/create/"
      targetPage = "/functions/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      bind "Base.Controller.Functions" "moduleName" "Module"
      bind "Base.Controller.Functions" "functionName" "function"
      bind "Base.Controller.Functions" "body" ""
      bind "Base.Controller.Functions" "parameters" ([] :: [String])
      outputFunctionPage currentPage targetPage Nothing Nothing


outputFunctionPage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> FruitTart ()
outputFunctionPage currentPage targetPage maybeWarning maybeFunctionID = do
  TemplateString moduleName
      <- getBinding "Base.Controller.Functions" "moduleName" >>= return . fromJust
  TemplateString functionName
      <- getBinding "Base.Controller.Functions" "functionName" >>= return . fromJust
  bind "Templates" "pageTitle" $ moduleName ++ "." ++ functionName
  pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                               [TemplateString "Base.Functions"]
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Base.Controller.Functions" "targetPage" targetPage
  bind "Base.Controller.Functions" "maybeWarning" maybeWarning
  bind "Base.Controller.Functions" "maybeFunctionID" maybeFunctionID
  pageContent <- getTemplate "Base.Controller.Functions" "function" []
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript" []
  fPutStr page


createPOST :: FruitTart ()
createPOST = do
  let currentPage = "/functions/create/"
      targetPage = "/functions/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeModuleName <- getInput "module"
      moduleName <- return $ case maybeModuleName of
                      Nothing -> "Module"
                      Just moduleName -> moduleName
      maybeFunctionName <- getInput "name"
      functionName <- return $ case maybeFunctionName of
                      Nothing -> "function"
                      Just functionName -> functionName
      maybeBody <- getInput "body"
      body <- return $ case maybeBody of
                         Nothing -> ""
                         Just body -> body
      parameters <- getInputParameters
      namedQuery "Queries" "beginExclusiveTransaction" []
      [values]
          <- namedQuery "Base.Controller.Functions" "functionExists"
                        [SQLText moduleName, SQLText functionName]
      exists <- return $ fromJust $ Map.lookup ("Base.Controller.Functions", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Controller.Functions" "insertFunction"
                     [SQLText moduleName,
                      SQLText functionName,
                      SQLText body]
          [values] <- namedQuery "Base.Controller.Functions" "functionJustInserted" []
          functionID <- return $ fromJust $ Map.lookup ("Base.Controller.Functions", "functionID")
                                                    values
          functionID <- return $ case functionID of
                                TemplateInteger integer -> integer
          mapM_ (\(parameterName, index) -> do
                   namedQuery "Base.Controller.Functions" "insertFunctionParameter"
                              [SQLInteger functionID,
                               SQLInteger index,
                               SQLText parameterName])
                $ zip parameters [0..]
          namedQuery "Queries" "commit" []
          setPopupMessage $ Just "Function created."
          seeOtherRedirect $ "/functions/view/" ++ (show functionID) ++ "/"
        True -> do
          namedQuery "Queries" "rollback" []
          bind "Base.Controller.Functions" "moduleName" moduleName
          bind "Base.Controller.Functions" "functionName" functionName
          bind "Base.Controller.Functions" "body" body
          outputFunctionPage currentPage targetPage
                          (Just "A function by that name already exists.")
                          Nothing


edit :: Int64 -> FruitTart ()
edit functionID = do
  let currentPage = "/functions/edit/" ++ (show functionID) ++ "/"
      targetPage = "/functions/edit/" ++ (show functionID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeModuleName <- getInput "module"
      moduleName <- return $ case maybeModuleName of
                      Nothing -> "Module"
                      Just moduleName -> moduleName
      maybeFunctionName <- getInput "name"
      functionName <- return $ case maybeFunctionName of
                      Nothing -> "function"
                      Just functionName -> functionName
      maybeBody <- getInput "body"
      body <- return $ case maybeBody of
                         Nothing -> ""
                         Just body -> body
      parameters <- getInputParameters
      namedQuery "Queries" "beginTransaction" []
      [values]
          <- namedQuery "Base.Controller.Functions" "functionExistsWithDifferentID"
                        [SQLText moduleName, SQLText functionName, SQLInteger functionID]
      exists <- return $ fromJust $ Map.lookup ("Base.Controller.Functions", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Controller.Functions" "updateFunction"
                     [SQLText moduleName,
                      SQLText functionName,
                      SQLText body,
                      SQLInteger functionID]
          namedQuery "Base.Controller.Functions" "deleteFunctionParameters" [SQLInteger functionID]
          mapM_ (\(parameterName, index) -> do
                   namedQuery "Base.Controller.Functions" "insertFunctionParameter"
                              [SQLInteger functionID,
                               SQLInteger index,
                               SQLText parameterName])
                $ zip parameters [0..]
          namedQuery "Queries" "commit" []
          setPopupMessage $ Just "Function changed."
          seeOtherRedirect $ "/functions/view/" ++ (show functionID) ++ "/"
        True -> do
          namedQuery "Queries" "rollback" []
          bind "Base.Controller.Functions" "moduleName" moduleName
          bind "Base.Controller.Functions" "functionName" functionName
          bind "Base.Controller.Functions" "body" body
          outputFunctionPage currentPage targetPage
                          (Just "A function by that name already exists.")
                          (Just functionID)


deleteGET :: Int64 -> FruitTart ()
deleteGET functionID = do
  let currentPage = "/functions/delete/" ++ (show functionID) ++ "/"
      targetPage = "/functions/delete/" ++ (show functionID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      bind "Templates" "pageTitle" "Delete Confirmation"
      pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                                   [TemplateString "Base.Functions"]
      bind "Templates" "pageHeadItems" pageHeadItems
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bindNamedQuery "Base.Controller.Functions" "functionName" [SQLInteger functionID]
      bind "Base.Controller.Functions" "targetPage" targetPage
      pageContent <- getTemplate "Base.Controller.Functions" "delete" []
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page" []
      fPutStr page


deletePOST :: Int64 -> FruitTart ()
deletePOST functionID = do
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage "/functions/index/"
    True -> do
      namedQuery "Queries" "beginTransaction" []
      namedQuery "Base.Controller.Functions" "deleteFunction" [SQLInteger functionID]
      namedQuery "Base.Controller.Functions" "deleteFunctionParameters" [SQLInteger functionID]
      namedQuery "Queries" "commit" []
      setPopupMessage $ Just "Function deleted."
      seeOtherRedirect "/functions/index/"


getInputParameters :: FruitTart [String]
getInputParameters
    = let getInputParameter index = do
            maybeParameter <- getInput $ "parameter" ++ (show index)
            parameter <- case maybeParameter of
                          Nothing -> return ""
                          Just parameter -> return parameter
            return $ Just parameter
          getInputParametersFrom index = do
            maybeParameter <- getInputParameter index
            case maybeParameter of
              Nothing -> return []
              Just parameter -> do
                rest <- getInputParametersFrom $ index + 1
                return $ parameter : rest
      in getInputParametersFrom 1

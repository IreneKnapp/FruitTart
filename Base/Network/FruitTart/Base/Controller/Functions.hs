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
      bind "Base" "pageTitle" "All Functions"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Base.Functions"]
      bind "Base" "pageHeadItems" pageHeadItems
      currentPage <- return $ "/functions/index/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Base.Functions" "indexRows" []
      pageContent <- getTemplate "Base.Functions" "index" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
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
          <- namedQuery "Base.Functions" "functionDetails"
                        [SQLInteger functionID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          functionName <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                          "functionName")
                                                         values
          functionName <- return $ case functionName of
                                  TemplateString string -> string
          body <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Functions" "moduleName" moduleName
          bind "Base.Functions" "functionName" functionName
          bind "Base.Functions" "body" body
          bindNamedQueryMultipleRows "Base.Functions"
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
          <- namedQuery "Base.Functions" "functionDetails"
                        [SQLInteger functionID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          functionName <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                          "functionName")
                                                         values
          functionName <- return $ case functionName of
                                  TemplateString string -> string
          body <- return $ fromJust $ Map.lookup ("Base.Functions",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Functions" "moduleName" moduleName
          bind "Base.Functions" "functionName" functionName
          bind "Base.Function" "body" body
          bindNamedQueryMultipleRows "Base.Function"
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
      bind "Base.Functions" "moduleName" "Module"
      bind "Base.Functions" "functionName" "function"
      bind "Base.Functions" "body" ""
      bind "Base.Functions" "parameters" ([] :: [String])
      outputFunctionPage currentPage targetPage Nothing Nothing


outputFunctionPage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> FruitTart ()
outputFunctionPage currentPage targetPage maybeWarning maybeFunctionID = do
  TemplateString moduleName
      <- getBinding "Base.Functions" "moduleName" >>= return . fromJust
  TemplateString functionName
      <- getBinding "Base.Functions" "functionName" >>= return . fromJust
  bind "Base" "pageTitle" $ moduleName ++ "." ++ functionName
  pageHeadItems <- getTemplate "Base" "pageHeadItems"
                               [TemplateString "Base.Functions"]
  bind "Base" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
  bind "Base" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Base" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Base" "popupMessage" popupMessage
  bind "Base.Functions" "targetPage" targetPage
  bind "Base" "maybeWarning" maybeWarning
  bind "Base.Functions" "maybeFunctionID" maybeFunctionID
  pageContent <- getTemplate "Base.Functions" "function" []
  bind "Base" "pageContent" pageContent
  page <- getTemplate "Base" "pageNoScript" []
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
      namedQuery "Base" "beginExclusiveTransaction" []
      [values]
          <- namedQuery "Base.Functions" "functionExists"
                        [SQLText moduleName, SQLText functionName]
      exists <- return $ fromJust $ Map.lookup ("Base.Functions", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Functions" "insertFunction"
                     [SQLText moduleName,
                      SQLText functionName,
                      SQLText body]
          [values] <- namedQuery "Base.Functions" "functionJustInserted" []
          functionID <- return $ fromJust $ Map.lookup ("Base.Functions", "functionID")
                                                    values
          functionID <- return $ case functionID of
                                TemplateInteger integer -> integer
          mapM_ (\(parameterName, index) -> do
                   namedQuery "Base.Functions" "insertFunctionParameter"
                              [SQLInteger functionID,
                               SQLInteger index,
                               SQLText parameterName])
                $ zip parameters [0..]
          namedQuery "Base" "commit" []
          setPopupMessage $ Just "Function created."
          seeOtherRedirect $ "/functions/view/" ++ (show functionID) ++ "/"
        True -> do
          namedQuery "Base" "rollback" []
          bind "Base.Functions" "moduleName" moduleName
          bind "Base.Functions" "functionName" functionName
          bind "Base.Functions" "body" body
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
      namedQuery "Base" "beginTransaction" []
      [values]
          <- namedQuery "Base.Functions" "functionExistsWithDifferentID"
                        [SQLText moduleName, SQLText functionName, SQLInteger functionID]
      exists <- return $ fromJust $ Map.lookup ("Base.Functions", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Functions" "updateFunction"
                     [SQLText moduleName,
                      SQLText functionName,
                      SQLText body,
                      SQLInteger functionID]
          namedQuery "Base.Functions" "deleteFunctionParameters" [SQLInteger functionID]
          mapM_ (\(parameterName, index) -> do
                   namedQuery "Base.Functions" "insertFunctionParameter"
                              [SQLInteger functionID,
                               SQLInteger index,
                               SQLText parameterName])
                $ zip parameters [0..]
          namedQuery "Base" "commit" []
          setPopupMessage $ Just "Function changed."
          seeOtherRedirect $ "/functions/view/" ++ (show functionID) ++ "/"
        True -> do
          namedQuery "Base" "rollback" []
          bind "Base.Functions" "moduleName" moduleName
          bind "Base.Functions" "functionName" functionName
          bind "Base.Functions" "body" body
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
      bind "Base" "pageTitle" "Delete Confirmation"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Base.Functions"]
      bind "Base" "pageHeadItems" pageHeadItems
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bindNamedQuery "Base.Functions" "functionName" [SQLInteger functionID]
      bind "Base.Functions" "targetPage" targetPage
      pageContent <- getTemplate "Base.Functions" "delete" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
      fPutStr page


deletePOST :: Int64 -> FruitTart ()
deletePOST functionID = do
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage "/functions/index/"
    True -> do
      namedQuery "Base" "beginTransaction" []
      namedQuery "Base.Functions" "deleteFunction" [SQLInteger functionID]
      namedQuery "Base.Functions" "deleteFunctionParameters" [SQLInteger functionID]
      namedQuery "Base" "commit" []
      setPopupMessage $ Just "Function deleted."
      seeOtherRedirect "/functions/index/"


getInputParameters :: FruitTart [String]
getInputParameters
    = let getInputParameter index = do
            getInput $ "parameter" ++ (show index)
          getInputParametersFrom index = do
            maybeParameter <- getInputParameter index
            case maybeParameter of
              Nothing -> return []
              Just parameter -> do
                rest <- getInputParametersFrom $ index + 1
                return $ parameter : rest
      in getInputParametersFrom 1

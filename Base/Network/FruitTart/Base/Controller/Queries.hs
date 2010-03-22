module Network.FruitTart.Base.Controller.Queries (
                                                  actionTable,
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
    False -> outputMustLoginPage "/queries/index/"
    True -> do
      bind "Templates" "pageTitle" "All Queries"
      pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                                   [TemplateString "Base.Queries"]
      bind "Templates" "pageHeadItems" pageHeadItems
      currentPage <- return $ "/queries/index/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Base.Controller.Queries"
                                 "indexRows"
                                 []
      pageContent <- getTemplate "Base.Controller.Queries" "index" []
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page" []
      fPutStr page


view :: Int64 -> FruitTart ()
view queryID = do
  let currentPage = "/queries/view/" ++ (show queryID) ++ "/"
      targetPage = "/queries/edit/" ++ (show queryID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeNames
          <- namedQuery "Base.Controller.Queries" "queryDetails" [SQLInteger queryID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          queryName <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                       "queryName")
                                                       values
          queryName <- return $ case queryName of
                                  TemplateString string -> string
          isTemplateExpression <- return $ fromJust $ Map.lookup
                                  ("Base.Controller.Queries",
                                   "isTemplateExpression")
                                  values
          isTemplateExpression <- return $ case isTemplateExpression of
                                             TemplateBool bool -> bool
          body <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Controller.Queries" "moduleName" moduleName
          bind "Base.Controller.Queries" "queryName" queryName
          bind "Base.Controller.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Controller.Queries" "body" body
          bindNamedQueryMultipleRows "Base.Controller.Queries"
                                     "results"
                                     [SQLInteger queryID]
          outputQueryPage currentPage targetPage Nothing (Just queryID)
        [] -> errorInvalidID "query"


copy :: Int64 -> FruitTart ()
copy queryID = do
  let currentPage = "/queries/copy/" ++ (show queryID) ++ "/"
      targetPage = "/queries/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeNames
          <- namedQuery "Base.Controller.Queries" "queryDetails" [SQLInteger queryID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          queryName <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                       "queryName")
                                                       values
          queryName <- return $ case queryName of
                                  TemplateString string -> string
          isTemplateExpression <- return $ fromJust $ Map.lookup
                                  ("Base.Controller.Queries",
                                   "isTemplateExpression")
                                  values
          isTemplateExpression <- return $ case isTemplateExpression of
                                             TemplateBool bool -> bool
          body <- return $ fromJust $ Map.lookup ("Base.Controller.Queries",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Controller.Queries" "moduleName" moduleName
          bind "Base.Controller.Queries" "queryName" queryName
          bind "Base.Controller.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Controller.Queries" "body" body
          bindNamedQueryMultipleRows "Base.Controller.Queries"
                                     "results"
                                     [SQLInteger queryID]
          outputQueryPage currentPage targetPage Nothing Nothing
        [] -> errorInvalidID "query"


createGET :: FruitTart ()
createGET = do
  let currentPage = "/queries/create/"
      targetPage = "/queries/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      bind "Base.Controller.Queries" "moduleName" "Module"
      bind "Base.Controller.Queries" "queryName" "query"
      bind "Base.Controller.Queries" "isTemplateExpression" False
      bind "Base.Controller.Queries" "body" ""
      bind "Base.Controller.Queries" "results" ([] :: [String])
      outputQueryPage currentPage targetPage Nothing Nothing


outputQueryPage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> FruitTart ()
outputQueryPage currentPage targetPage maybeWarning maybeQueryID = do
  TemplateString moduleName
      <- getBinding "Base.Controller.Queries" "moduleName" >>= return . fromJust
  TemplateString queryName
      <- getBinding "Base.Controller.Queries" "queryName" >>= return . fromJust
  bind "Templates" "pageTitle" $ moduleName ++ "." ++ queryName
  pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                               [TemplateString "Base.Queries"]
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Base.Controller.Queries" "targetPage" targetPage
  bind "Base.Controller.Queries" "maybeWarning" maybeWarning
  bind "Base.Controller.Queries" "maybeQueryID" maybeQueryID
  pageContent <- getTemplate "Base.Controller.Queries" "query" []
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript" []
  fPutStr page


createPOST :: FruitTart ()
createPOST = do
  let currentPage = "/queries/create/"
      targetPage = "/queries/create/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeModuleName <- getInput "module"
      moduleName <- return $ case maybeModuleName of
                      Nothing -> "Module"
                      Just moduleName -> moduleName
      maybeQueryName <- getInput "name"
      queryName <- return $ case maybeQueryName of
                      Nothing -> "query"
                      Just queryName -> queryName
      maybeIsTemplateExpression <- getInput "is-template-expression"
      isTemplateExpression <- return $ case maybeIsTemplateExpression of
                                         Nothing -> False
                                         Just _ -> True
      maybeBody <- getInput "body"
      body <- return $ case maybeBody of
                         Nothing -> ""
                         Just body -> body
      items <- getInputItems
      namedQuery "Queries" "beginExclusiveTransaction" []
      [values]
          <- namedQuery "Base.Controller.Queries" "queryExists"
                        [SQLText moduleName, SQLText queryName]
      exists <- return $ fromJust $ Map.lookup ("Base.Controller.Queries", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Controller.Queries" "insertQuery"
                     [SQLText moduleName,
                      SQLText queryName,
                      SQLInteger $ if isTemplateExpression then 1 else 0,
                      SQLText body]
          [values] <- namedQuery "Base.Controller.Queries" "queryJustInserted" []
          queryID <- return $ fromJust $ Map.lookup ("Base.Controller.Queries", "queryID")
                                                    values
          queryID <- return $ case queryID of
                                TemplateInteger integer -> integer
          if not isTemplateExpression
            then mapM (\((itemType, itemName), index) -> do
                         namedQuery "Base.Controller.Queries" "insertQueryResult"
                                    [SQLInteger queryID,
                                     SQLInteger index,
                                     SQLText itemType,
                                     SQLText itemName])
                 $ zip items [0..]
            else return []
          namedQuery "Queries" "commit" []
          setPopupMessage $ Just "Query created."
          seeOtherRedirect $ "/queries/view/" ++ (show queryID) ++ "/"
        True -> do
          namedQuery "Queries" "rollback" []
          bind "Base.Controller.Queries" "moduleName" moduleName
          bind "Base.Controller.Queries" "queryName" queryName
          bind "Base.Controller.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Controller.Queries" "body" body
          outputQueryPage currentPage targetPage
                          (Just "A query by that name already exists.")
                          Nothing


edit :: Int64 -> FruitTart ()
edit queryID = do
  let currentPage = "/queries/edit/" ++ (show queryID) ++ "/"
      targetPage = "/queries/edit/" ++ (show queryID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      maybeModuleName <- getInput "module"
      moduleName <- return $ case maybeModuleName of
                      Nothing -> "Module"
                      Just moduleName -> moduleName
      maybeQueryName <- getInput "name"
      queryName <- return $ case maybeQueryName of
                      Nothing -> "query"
                      Just queryName -> queryName
      maybeIsTemplateExpression <- getInput "is-template-expression"
      isTemplateExpression <- return $ case maybeIsTemplateExpression of
                                         Nothing -> False
                                         Just _ -> True
      maybeBody <- getInput "body"
      body <- return $ case maybeBody of
                         Nothing -> ""
                         Just body -> body
      items <- getInputItems
      namedQuery "Queries" "beginTransaction" []
      [values]
          <- namedQuery "Base.Controller.Queries" "queryExistsWithDifferentID"
                        [SQLText moduleName, SQLText queryName, SQLInteger queryID]
      exists <- return $ fromJust $ Map.lookup ("Base.Controller.Queries", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Controller.Queries" "updateQuery"
                     [SQLText moduleName,
                      SQLText queryName,
                      SQLInteger $ if isTemplateExpression then 1 else 0,
                      SQLText body,
                      SQLInteger queryID]
          namedQuery "Base.Controller.Queries" "deleteQueryResults" [SQLInteger queryID]
          if not isTemplateExpression
            then mapM (\((itemType, itemName), index) -> do
                         namedQuery "Base.Controller.Queries" "insertQueryResult"
                                    [SQLInteger queryID,
                                     SQLInteger index,
                                     SQLText itemType,
                                     SQLText itemName])
                      $ zip items [0..]
            else return []
          namedQuery "Queries" "commit" []
          setPopupMessage $ Just "Query changed."
          seeOtherRedirect $ "/queries/view/" ++ (show queryID) ++ "/"
        True -> do
          namedQuery "Queries" "rollback" []
          bind "Base.Controller.Queries" "moduleName" moduleName
          bind "Base.Controller.Queries" "queryName" queryName
          bind "Base.Controller.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Controller.Queries" "body" body
          outputQueryPage currentPage targetPage
                          (Just "A query by that name already exists.")
                          (Just queryID)


deleteGET :: Int64 -> FruitTart ()
deleteGET queryID = do
  let currentPage = "/queries/delete/" ++ (show queryID) ++ "/"
      targetPage = "/queries/delete/" ++ (show queryID) ++ "/"
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage currentPage
    True -> do
      bind "Templates" "pageTitle" "Delete Confirmation"
      pageHeadItems <- getTemplate "Templates" "pageHeadItems"
                                   [TemplateString "Base.Queries"]
      bind "Templates" "pageHeadItems" pageHeadItems
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Templates" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Templates" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Templates" "popupMessage" popupMessage
      bindNamedQuery "Base.Controller.Queries" "queryName" [SQLInteger queryID]
      bind "Base.Controller.Queries" "targetPage" targetPage
      pageContent <- getTemplate "Base.Controller.Queries" "delete" []
      bind "Templates" "pageContent" pageContent
      page <- getTemplate "Templates" "page" []
      fPutStr page


deletePOST :: Int64 -> FruitTart ()
deletePOST queryID = do
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage "/queries/index/"
    True -> do
      namedQuery "Queries" "beginTransaction" []
      namedQuery "Base.Controller.Queries" "deleteQuery" [SQLInteger queryID]
      namedQuery "Base.Controller.Queries" "deleteQueryResults" [SQLInteger queryID]
      namedQuery "Queries" "commit" []
      setPopupMessage $ Just "Query deleted."
      seeOtherRedirect "/queries/index/"


getInputItems :: FruitTart [(String, String)]
getInputItems
    = let getInputItem index = do
            maybeItemType <- getInput $ "type" ++ (show index)
            case maybeItemType of
              Nothing -> return Nothing
              Just itemType -> do
                maybeItemName <- getInput $ "name" ++ (show index)
                itemName <- case maybeItemName of
                          Nothing -> return ""
                          Just itemName -> return itemName
                return $ Just (itemType, itemName)
          getInputItemsFrom index = do
            maybeItem <- getInputItem index
            case maybeItem of
              Nothing -> return []
              Just item -> do
                rest <- getInputItemsFrom $ index + 1
                return $ item : rest
      in getInputItemsFrom 1

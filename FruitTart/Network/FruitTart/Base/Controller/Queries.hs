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
      bind "Base" "pageTitle" "All Queries"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Base.Queries"]
      bind "Base" "pageHeadItems" pageHeadItems
      currentPage <- return $ "/queries/index/"
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bindNamedQueryMultipleRows "Base.Queries"
                                 "indexRows"
                                 []
      pageContent <- getTemplate "Base.Queries" "index" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
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
          <- namedQuery "Base.Queries" "queryDetails" [SQLInteger queryID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          queryName <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                       "queryName")
                                                       values
          queryName <- return $ case queryName of
                                  TemplateString string -> string
          isTemplateExpression <- return $ fromJust $ Map.lookup
                                  ("Base.Queries",
                                   "isTemplateExpression")
                                  values
          isTemplateExpression <- return $ case isTemplateExpression of
                                             TemplateBool bool -> bool
          body <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Queries" "moduleName" moduleName
          bind "Base.Queries" "queryName" queryName
          bind "Base.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Queries" "body" body
          bindNamedQueryMultipleRows "Base.Queries"
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
          <- namedQuery "Base.Queries" "queryDetails" [SQLInteger queryID]
      case maybeNames of
        [values] -> do
          moduleName <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                        "moduleName")
                                                       values
          moduleName <- return $ case moduleName of
                                   TemplateString string -> string
          queryName <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                       "queryName")
                                                       values
          queryName <- return $ case queryName of
                                  TemplateString string -> string
          isTemplateExpression <- return $ fromJust $ Map.lookup
                                  ("Base.Queries",
                                   "isTemplateExpression")
                                  values
          isTemplateExpression <- return $ case isTemplateExpression of
                                             TemplateBool bool -> bool
          body <- return $ fromJust $ Map.lookup ("Base.Queries",
                                                  "body")
                                                 values
          body <- return $ case body of
                             TemplateString body -> body
          bind "Base.Queries" "moduleName" moduleName
          bind "Base.Queries" "queryName" queryName
          bind "Base.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Queries" "body" body
          bindNamedQueryMultipleRows "Base.Queries"
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
      bind "Base.Queries" "moduleName" "Module"
      bind "Base.Queries" "queryName" "query"
      bind "Base.Queries" "isTemplateExpression" False
      bind "Base.Queries" "body" ""
      bind "Base.Queries" "results" ([] :: [String])
      outputQueryPage currentPage targetPage Nothing Nothing


outputQueryPage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> FruitTart ()
outputQueryPage currentPage targetPage maybeWarning maybeQueryID = do
  TemplateString moduleName
      <- getBinding "Base.Queries" "moduleName" >>= return . fromJust
  TemplateString queryName
      <- getBinding "Base.Queries" "queryName" >>= return . fromJust
  bind "Base" "pageTitle" $ moduleName ++ "." ++ queryName
  pageHeadItems <- getTemplate "Base" "pageHeadItems"
                               [TemplateString "Base.Queries"]
  bind "Base" "pageHeadItems" pageHeadItems
  navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
  bind "Base" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Base" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Base" "popupMessage" popupMessage
  bind "Base.Queries" "targetPage" targetPage
  bind "Base" "maybeWarning" maybeWarning
  bind "Base.Queries" "maybeQueryID" maybeQueryID
  pageContent <- getTemplate "Base.Queries" "query" []
  bind "Base" "pageContent" pageContent
  page <- getTemplate "Base" "pageNoScript" []
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
      results <- getInputResults
      namedQuery "Base" "beginExclusiveTransaction" []
      [values]
          <- namedQuery "Base.Queries" "queryExists"
                        [SQLText moduleName, SQLText queryName]
      exists <- return $ fromJust $ Map.lookup ("Base.Queries", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Queries" "insertQuery"
                     [SQLText moduleName,
                      SQLText queryName,
                      SQLInteger $ if isTemplateExpression then 1 else 0,
                      SQLText body]
          [values] <- namedQuery "Base.Queries" "queryJustInserted" []
          queryID <- return $ fromJust $ Map.lookup ("Base.Queries", "queryID")
                                                    values
          queryID <- return $ case queryID of
                                TemplateInteger integer -> integer
          if not isTemplateExpression
            then mapM (\((resultType, resultName), index) -> do
                         namedQuery "Base.Queries" "insertQueryResult"
                                    [SQLInteger queryID,
                                     SQLInteger index,
                                     SQLText resultType,
                                     SQLText resultName])
                 $ zip results [0..]
            else return []
          namedQuery "Base" "commit" []
          setPopupMessage $ Just "Query created."
          seeOtherRedirect $ "/queries/view/" ++ (show queryID) ++ "/"
        True -> do
          namedQuery "Base" "rollback" []
          bind "Base.Queries" "moduleName" moduleName
          bind "Base.Queries" "queryName" queryName
          bind "Base.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Queries" "body" body
          bind "Base.Queries" "results"
               [Map.fromList
                $ concat
                  $ map (\((resultType, resultName), index)
                           -> [(("Base.Queries", "name"),
                                TemplateString resultName),
                               (("Base.Queries", "type"),
                                TemplateString resultType),
                               (("Base.Queries", "index"),
                                TemplateInteger index)])
                        $ zip results [0..]]
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
      results <- getInputResults
      namedQuery "Base" "beginTransaction" []
      [values]
          <- namedQuery "Base.Queries" "queryExistsWithDifferentID"
                        [SQLText moduleName, SQLText queryName, SQLInteger queryID]
      exists <- return $ fromJust $ Map.lookup ("Base.Queries", "exists")
                                               values
      exists <- return $ case exists of
                           TemplateBool bool -> bool
      case exists of
        False -> do
          namedQuery "Base.Queries" "updateQuery"
                     [SQLText moduleName,
                      SQLText queryName,
                      SQLInteger $ if isTemplateExpression then 1 else 0,
                      SQLText body,
                      SQLInteger queryID]
          namedQuery "Base.Queries" "deleteQueryResults" [SQLInteger queryID]
          if not isTemplateExpression
            then mapM (\((resultType, resultName), index) -> do
                         namedQuery "Base.Queries" "insertQueryResult"
                                    [SQLInteger queryID,
                                     SQLInteger index,
                                     SQLText resultType,
                                     SQLText resultName])
                      $ zip results [0..]
            else return []
          namedQuery "Base" "commit" []
          setPopupMessage $ Just "Query changed."
          seeOtherRedirect $ "/queries/view/" ++ (show queryID) ++ "/"
        True -> do
          namedQuery "Base" "rollback" []
          bind "Base.Queries" "moduleName" moduleName
          bind "Base.Queries" "queryName" queryName
          bind "Base.Queries" "isTemplateExpression" isTemplateExpression
          bind "Base.Queries" "body" body
          bind "Base.Queries" "results"
               [Map.fromList
                $ concat
                  $ map (\((resultType, resultName), index)
                           -> [(("Base.Queries", "name"),
                                TemplateString resultName),
                               (("Base.Queries", "type"),
                                TemplateString resultType),
                               (("Base.Queries", "index"),
                                TemplateInteger index)])
                        $ zip results [0..]]
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
      bind "Base" "pageTitle" "Delete Confirmation"
      pageHeadItems <- getTemplate "Base" "pageHeadItems"
                                   [TemplateString "Base.Queries"]
      bind "Base" "pageHeadItems" pageHeadItems
      navigationBar <- getTemplate "Base" "navigationBar" [TemplateString currentPage]
      bind "Base" "navigationBar" navigationBar
      loginButton <- getLoginButton currentPage
      bind "Base" "loginButton" loginButton
      popupMessage <- getPopupMessage
      bind "Base" "popupMessage" popupMessage
      bindNamedQuery "Base.Queries" "queryName" [SQLInteger queryID]
      bind "Base.Queries" "targetPage" targetPage
      pageContent <- getTemplate "Base.Queries" "delete" []
      bind "Base" "pageContent" pageContent
      page <- getTemplate "Base" "page" []
      fPutStr page


deletePOST :: Int64 -> FruitTart ()
deletePOST queryID = do
  right <- getRightAdminDesign
  case right of
    False -> outputMustLoginPage "/queries/index/"
    True -> do
      namedQuery "Base" "beginTransaction" []
      namedQuery "Base.Queries" "deleteQuery" [SQLInteger queryID]
      namedQuery "Base.Queries" "deleteQueryResults" [SQLInteger queryID]
      namedQuery "Base" "commit" []
      setPopupMessage $ Just "Query deleted."
      seeOtherRedirect "/queries/index/"


getInputResults :: FruitTart [(String, String)]
getInputResults
    = let getInputResult index = do
            maybeResultType <- getInput $ "type" ++ (show index)
            case maybeResultType of
              Nothing -> return Nothing
              Just resultType -> do
                maybeResultName <- getInput $ "name" ++ (show index)
                resultName <- case maybeResultName of
                          Nothing -> return ""
                          Just resultName -> return resultName
                return $ Just (resultType, resultName)
          getInputResultsFrom index = do
            maybeResult <- getInputResult index
            case maybeResult of
              Nothing -> return []
              Just result -> do
                rest <- getInputResultsFrom $ index + 1
                return $ result : rest
      in getInputResultsFrom 1

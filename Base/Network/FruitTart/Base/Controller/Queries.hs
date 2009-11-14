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
import Network.FruitTart.Base.View.Navigation
import Network.FruitTart.Base.View.PopupMessage
import Network.FruitTart.Base.View.Templates
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view),
                       ("create", "GET", [], [], toDyn createGET),
                       ("create", "POST", [], [], toDyn createPOST),
                       ("edit", "POST", [IDParameter], [], toDyn edit),
                       ("copy", "GET", [IDParameter], [], toDyn copy),
                       ("delete", "GET", [IDParameter], [], toDyn deleteGET),
                       ("delete", "POST", [IDParameter], [], toDyn deletePOST)]


index :: FruitTart CGIResult
index = do
  bind "Templates" "pageTitle" "All Queries"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  currentPage <- return $ "/queries/index/"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bindQueryMultipleRows "Base.Controller.Queries"
                        "rows"
                        [("queryID", TInt),
                         ("moduleName", TString),
                         ("queryName", TString)]
                        "SELECT id, module, name FROM queries ORDER BY module, name"
                        []
  pageContent <- getTemplate "Base.Controller.Queries" "index"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


view :: Int64 -> FruitTart CGIResult
view queryID = do
  let currentPage = "/queries/view/" ++ (show queryID) ++ "/"
      targetPage = "/queries/edit/" ++ (show queryID) ++ "/"
  maybeNames
      <- query "SELECT module, name, body FROM queries WHERE id = ?"
               [SQLInteger queryID]
  case maybeNames of
    [[SQLText moduleName, SQLText queryName, SQLText body]] -> do
      bind "Base.Controller.Queries" "moduleName" moduleName
      bind "Base.Controller.Queries" "queryName" queryName
      bind "Base.Controller.Queries" "body" body
      bind "Base.Controller.Queries" "rowCount"
           $ TemplateInteger $ fromIntegral $ findRowCount body
      bindQueryMultipleRows "Base.Controller.Queries"
                            "results"
                            [("name", TString),
                             ("type", TString),
                             ("index", TInt)]
                            ("SELECT name, type, item FROM query_results "
                             ++ "WHERE query = ? ORDER BY item")
                            [SQLInteger queryID]
      outputQueryPage currentPage targetPage Nothing (Just queryID)
    [] -> errorInvalidID "query"


copy :: Int64 -> FruitTart CGIResult
copy queryID = do
  let currentPage = "/queries/copy/" ++ (show queryID) ++ "/"
      targetPage = "/queries/create/"
  maybeNames
      <- query "SELECT module, name, body FROM queries WHERE id = ?"
               [SQLInteger queryID]
  case maybeNames of
    [[SQLText moduleName, SQLText queryName, SQLText body]] -> do
      bind "Base.Controller.Queries" "moduleName" moduleName
      bind "Base.Controller.Queries" "queryName" queryName
      bind "Base.Controller.Queries" "body" body
      bind "Base.Controller.Queries" "rowCount"
           $ TemplateInteger $ fromIntegral $ findRowCount body
      bindQueryMultipleRows "Base.Controller.Queries"
                            "results"
                            [("name", TString),
                             ("type", TString),
                             ("index", TInt)]
                            ("SELECT name, type, item FROM query_results "
                             ++ "WHERE query = ? ORDER BY item")
                            [SQLInteger queryID]
      outputQueryPage currentPage targetPage Nothing Nothing
    [] -> errorInvalidID "query"


createGET :: FruitTart CGIResult
createGET = do
  let currentPage = "/queries/create/"
      targetPage = "/queries/create/"
  bind "Base.Controller.Queries" "moduleName" "Module"
  bind "Base.Controller.Queries" "queryName" "query"
  bind "Base.Controller.Queries" "body" ""
  bind "Base.Controller.Queries" "rowCount"
       $ TemplateInteger $ fromIntegral $ findRowCount ""
  bind "Base.Controller.Queries" "results" ([] :: [String])
  outputQueryPage currentPage targetPage Nothing Nothing


outputQueryPage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> FruitTart CGIResult
outputQueryPage currentPage targetPage maybeWarning maybeQueryID = do
  TemplateString moduleName
      <- getBinding "Base.Controller.Queries" "moduleName" >>= return . fromJust
  TemplateString queryName
      <- getBinding "Base.Controller.Queries" "queryName" >>= return . fromJust
  bind "Templates" "pageTitle" $ moduleName ++ "." ++ queryName
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" $ pageHeadItems
         ++ "<link href=\"/css/base.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "<script src=\"/js/queries.js\" type=\"text/ecmascript\"></script>\n"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Base.Controller.Queries" "targetPage" targetPage
  bind "Base.Controller.Queries" "maybeWarning" maybeWarning
  bind "Base.Controller.Queries" "maybeQueryID" maybeQueryID
  pageContent <- getTemplate "Base.Controller.Queries" "query"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript"
  output page


createPOST :: FruitTart CGIResult
createPOST = do
  let currentPage = "/queries/create/"
      targetPage = "/queries/create/"
  maybeModuleName <- getInput "module"
  moduleName <- return $ case maybeModuleName of
                  Nothing -> "Module"
                  Just moduleName -> moduleName
  maybeQueryName <- getInput "name"
  queryName <- return $ case maybeQueryName of
                  Nothing -> "query"
                  Just queryName -> queryName
  maybeBody <- getInput "body"
  body <- return $ case maybeBody of
                     Nothing -> ""
                     Just body -> body
  items <- getInputItems
  query "BEGIN EXCLUSIVE TRANSACTION" []
  [[SQLInteger count]]
      <- query "SELECT count(*) FROM queries WHERE module = ? AND name = ?"
               [SQLText moduleName, SQLText queryName]
  case count of
    0 -> do
      query "INSERT INTO queries (module, name, body) VALUES (?, ?, ?)"
            [SQLText moduleName, SQLText queryName, SQLText body]
      [[SQLInteger queryID]] <- query "SELECT max(id) FROM queries" []
      mapM (\((itemType, itemName), index) -> do
              query ("INSERT INTO query_results (query, item, type, name) "
                     ++ "VALUES (?, ?, ?, ?)")
                    [SQLInteger queryID,
                     SQLInteger index,
                     SQLText itemType,
                     SQLText itemName])
           $ zip items [0..]
      query "COMMIT" []
      setPopupMessage $ Just "Query created."
      seeOtherRedirect $ "/queries/index/"
    _ -> do
      query "ROLLBACK" []
      bind "Base.Controller.Queries" "moduleName" moduleName
      bind "Base.Controller.Queries" "queryName" queryName
      bind "Base.Controller.Queries" "body" body
      bind "Base.Controller.Queries" "rowCount"
           $ TemplateInteger $ fromIntegral $ findRowCount body
      outputQueryPage currentPage targetPage
                      (Just "A query by that name already exists.")
                      Nothing


edit :: Int64 -> FruitTart CGIResult
edit queryID = do
  let currentPage = "/queries/edit/" ++ (show queryID) ++ "/"
      targetPage = "/queries/edit/" ++ (show queryID) ++ "/"
  maybeModuleName <- getInput "module"
  moduleName <- return $ case maybeModuleName of
                  Nothing -> "Module"
                  Just moduleName -> moduleName
  maybeQueryName <- getInput "name"
  queryName <- return $ case maybeQueryName of
                  Nothing -> "query"
                  Just queryName -> queryName
  maybeBody <- getInput "body"
  body <- return $ case maybeBody of
                     Nothing -> ""
                     Just body -> body
  items <- getInputItems
  query "BEGIN TRANSACTION" []
  [[SQLInteger count]]
      <- query "SELECT count(*) FROM queries WHERE module = ? AND name = ? AND id != ?"
               [SQLText moduleName, SQLText queryName, SQLInteger queryID]
  case count of
    0 -> do
      query "UPDATE queries SET module = ?, name = ?, body = ? WHERE id = ?"
            [SQLText moduleName, SQLText queryName, SQLText body, SQLInteger queryID]
      query "DELETE FROM query_results WHERE query = ?" [SQLInteger queryID]
      mapM (\((itemType, itemName), index) -> do
              query ("INSERT INTO query_results (query, item, type, name) "
                     ++ "VALUES (?, ?, ?, ?)")
                    [SQLInteger queryID,
                     SQLInteger index,
                     SQLText itemType,
                     SQLText itemName])
           $ zip items [0..]
      query "COMMIT" []
      setPopupMessage $ Just "Query changed."
      seeOtherRedirect $ "/queries/index/"
    _ -> do
      query "ROLLBACK" []
      bind "Base.Controller.Queries" "moduleName" moduleName
      bind "Base.Controller.Queries" "queryName" queryName
      bind "Base.Controller.Queries" "body" body
      bind "Base.Controller.Queries" "rowCount"
           $ TemplateInteger $ fromIntegral $ findRowCount body
      outputQueryPage currentPage targetPage
                      (Just "A query by that name already exists.")
                      (Just queryID)


deleteGET :: Int64 -> FruitTart CGIResult
deleteGET queryID = do
  let currentPage = "/queries/delete/" ++ (show queryID) ++ "/"
      targetPage = "/queries/delete/" ++ (show queryID) ++ "/"
  bind "Templates" "pageTitle" "Delete Confirmation"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bindQuery "Base.Controller.Queries"
            [("moduleName", TString),
             ("queryName", TString)]
            "SELECT module, name FROM queries WHERE id = ?"
            [SQLInteger queryID]
  bind "Base.Controller.Queries" "targetPage" targetPage
  pageContent <- getTemplate "Base.Controller.Queries" "delete"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


deletePOST :: Int64 -> FruitTart CGIResult
deletePOST queryID = do
  query "BEGIN TRANSACTION" []
  query "DELETE FROM queries WHERE id = ?" [SQLInteger queryID]
  query "DELETE FROM query_results WHERE query = ?" [SQLInteger queryID]
  query "COMMIT" []
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


findRowCount :: String -> Int
findRowCount body = length $ split '\n' $ wordWrap body 60

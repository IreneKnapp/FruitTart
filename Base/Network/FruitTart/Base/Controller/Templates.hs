module Network.FruitTart.Base.Controller.Templates (
                                                    actionTable,
                                                   )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
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
  bind "Templates" "pageTitle" "All Templates"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  currentPage <- return $ "/templates/index/"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bindQueryMultipleRows "Base.Controller.Templates"
                        "rows"
                        [("templateID", TInt),
                         ("moduleName", TString),
                         ("templateName", TString)]
                        "SELECT id, module, name FROM templates ORDER BY module, name"
                        []
  pageContent <- getTemplate "Base.Controller.Templates" "index"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


view :: Int64 -> FruitTart CGIResult
view templateID = do
  let currentPage = "/templates/view/" ++ (show templateID) ++ "/"
      targetPage = "/templates/edit/" ++ (show templateID) ++ "/"
  maybeNames
      <- query "SELECT module, name FROM templates WHERE id = ?"
               [SQLInteger templateID]
  case maybeNames of
    [[SQLText moduleName, SQLText templateName]] -> do
      sqlItems <- query ("SELECT kind, body FROM template_items "
                         ++ "WHERE template = ? ORDER BY item")
                        [SQLInteger templateID]
      let items = map (\[SQLText itemTypeName, SQLText body] ->
                        let itemType = case itemTypeName of
                                         "content" -> Content
                                         "expression" -> Expression
                                         _ -> Content
                        in (itemType, body))
                      sqlItems
      outputTemplatePage currentPage targetPage Nothing (Just templateID)
                         moduleName templateName items
    [] -> errorInvalidID "template"


copy :: Int64 -> FruitTart CGIResult
copy templateID = do
  let currentPage = "/templates/copy/" ++ (show templateID) ++ "/"
      targetPage = "/templates/create/"
  maybeNames
      <- query "SELECT module, name FROM templates WHERE id = ?"
               [SQLInteger templateID]
  case maybeNames of
    [[SQLText moduleName, SQLText templateName]] -> do
      sqlItems <- query ("SELECT kind, body FROM template_items "
                         ++ "WHERE template = ? ORDER BY item")
                        [SQLInteger templateID]
      let items = map (\[SQLText itemTypeName, SQLText body] ->
                        let itemType = case itemTypeName of
                                         "content" -> Content
                                         "expression" -> Expression
                                         _ -> Content
                        in (itemType, body))
                      sqlItems
      outputTemplatePage currentPage targetPage Nothing Nothing
                         moduleName templateName items
    [] -> errorInvalidID "template"


createGET :: FruitTart CGIResult
createGET = do
  let currentPage = "/templates/create/"
      targetPage = "/templates/create/"
  outputTemplatePage currentPage targetPage Nothing Nothing
                     "Module" "template" [(Content, "")]


outputTemplatePage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> String -> String
    -> [(TemplateItemType, String)]
    -> FruitTart CGIResult
outputTemplatePage currentPage targetPage maybeWarning maybeTemplateID
                   moduleName templateName bodies = do
  bind "Templates" "pageTitle" $ moduleName ++ "." ++ templateName
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" $ pageHeadItems
         ++ "<link href=\"/css/templates.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "<script src=\"/js/templates.js\" type=\"text/ecmascript\"></script>\n"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Base.Controller.Templates" "bodies"
       $ map (\((itemType, body), index)
                  -> Map.fromList [(("Base.Controller.Templates",
                                     "itemType"),
                                    TemplateString $ case itemType of
                                                       Content -> "content"
                                                       Expression -> "expression"),
                                   (("Base.Controller.Templates", "body"),
                                    TemplateString body),
                                   (("Base.Controller.Templates", "rowCount"),
                                    TemplateInteger $ fromIntegral $ findRowCount body),
                                   (("Base.Controller.Templates", "index"),
                                    TemplateInteger index)])
             $ zip bodies [1..]
  bind "Base.Controller.Templates" "targetPage" targetPage
  bind "Base.Controller.Templates" "maybeWarning" maybeWarning
  bind "Base.Controller.Templates" "maybeTemplateID" maybeTemplateID
  bind "Base.Controller.Templates" "moduleName" moduleName
  bind "Base.Controller.Templates" "templateName" templateName
  pageContent <- getTemplate "Base.Controller.Templates" "template"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "pageNoScript"
  output page


createPOST :: FruitTart CGIResult
createPOST = do
  let currentPage = "/templates/create/"
      targetPage = "/templates/create/"
  maybeModuleName <- getInput "module"
  moduleName <- return $ case maybeModuleName of
                  Nothing -> "Module"
                  Just moduleName -> moduleName
  maybeTemplateName <- getInput "name"
  templateName <- return $ case maybeTemplateName of
                  Nothing -> "template"
                  Just templateName -> templateName
  items <- getInputItems
  query "BEGIN EXCLUSIVE TRANSACTION" []
  [[SQLInteger count]]
      <- query "SELECT count(*) FROM templates WHERE module = ? AND name = ?"
               [SQLText moduleName, SQLText templateName]
  case count of
    0 -> do
      query "INSERT INTO templates (module, name) VALUES (?, ?)"
            [SQLText moduleName, SQLText templateName]
      [[SQLInteger templateID]] <- query "SELECT max(id) FROM templates" []
      mapM (\((itemType, body), index) -> do
              itemTypeName <- return $ case itemType of
                                Content -> "content"
                                Expression -> "expression"
              query ("INSERT INTO template_items (template, item, kind, body) "
                     ++ "VALUES (?, ?, ?, ?)")
                    [SQLInteger templateID,
                     SQLInteger index,
                     SQLText itemTypeName,
                     SQLText body])
           $ zip items [0..]
      query "COMMIT" []
      setPopupMessage $ Just "Template created."
      seeOtherRedirect $ "/templates/index/"
    _ -> do
      query "ROLLBACK" []
      outputTemplatePage currentPage targetPage
                         (Just "A template by that name already exists.")
                         Nothing moduleName templateName items


edit :: Int64 -> FruitTart CGIResult
edit templateID = do
  let currentPage = "/templates/edit/" ++ (show templateID) ++ "/"
      targetPage = "/templates/edit/" ++ (show templateID) ++ "/"
  maybeModuleName <- getInput "module"
  moduleName <- return $ case maybeModuleName of
                  Nothing -> "Module"
                  Just moduleName -> moduleName
  maybeTemplateName <- getInput "name"
  templateName <- return $ case maybeTemplateName of
                  Nothing -> "template"
                  Just templateName -> templateName
  items <- getInputItems
  query "BEGIN TRANSACTION" []
  [[SQLInteger count]]
      <- query "SELECT count(*) FROM templates WHERE module = ? AND name = ? AND id != ?"
               [SQLText moduleName, SQLText templateName, SQLInteger templateID]
  case count of
    0 -> do
      query "UPDATE templates SET module = ?, name = ? WHERE id = ?"
            [SQLText moduleName, SQLText templateName, SQLInteger templateID]
      query "DELETE FROM template_items WHERE template = ?" [SQLInteger templateID]
      mapM (\((itemType, body), index) -> do
              itemTypeName <- return $ case itemType of
                                Content -> "content"
                                Expression -> "expression"
              query ("INSERT INTO template_items (template, item, kind, body) "
                     ++ "VALUES (?, ?, ?, ?)")
                    [SQLInteger templateID,
                     SQLInteger index,
                     SQLText itemTypeName,
                     SQLText body])
           $ zip items [0..]
      query "COMMIT" []
      setPopupMessage $ Just "Template changed."
      seeOtherRedirect $ "/templates/index/"
    _ -> do
      query "ROLLBACK" []
      outputTemplatePage currentPage targetPage
                         (Just "A template by that name already exists.")
                         (Just templateID)
                         moduleName templateName items


deleteGET :: Int64 -> FruitTart CGIResult
deleteGET templateID = do
  let currentPage = "/templates/delete/" ++ (show templateID) ++ "/"
      targetPage = "/templates/delete/" ++ (show templateID) ++ "/"
  bind "Templates" "pageTitle" "Delete Confirmation"
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" pageHeadItems
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bindQuery "Base.Controller.Templates"
            [("moduleName", TString),
             ("templateName", TString)]
            "SELECT module, name FROM templates WHERE id = ?"
            [SQLInteger templateID]
  bind "Base.Controller.Templates" "targetPage" targetPage
  pageContent <- getTemplate "Base.Controller.Templates" "delete"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


deletePOST :: Int64 -> FruitTart CGIResult
deletePOST templateID = do
  query "BEGIN TRANSACTION" []
  query "DELETE FROM templates WHERE id = ?" [SQLInteger templateID]
  query "DELETE FROM template_items WHERE template = ?" [SQLInteger templateID]
  query "COMMIT" []
  setPopupMessage $ Just "Template deleted."
  seeOtherRedirect "/templates/index/"


getInputItems :: FruitTart [(TemplateItemType, String)]
getInputItems
    = let getInputItem index = do
            maybeItemTypeName <- getInput $ "type" ++ (show index)
            case maybeItemTypeName of
              Nothing -> return Nothing
              Just itemTypeName -> do
                let itemType = case itemTypeName of
                                 "content" -> Content
                                 "expression" -> Expression
                                 _ -> Content
                maybeBody <- getInput $ "body" ++ (show index)
                body <- case maybeBody of
                          Nothing -> return ""
                          Just body -> return body
                return $ Just (itemType, body)
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

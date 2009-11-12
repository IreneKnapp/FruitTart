module Network.FruitTart.Templates.Controller.Templates (
                                                         actionTable,
                                                         functionTable
                                                        )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import Network.FruitTart.Base
import Network.FruitTart.PluginInterface
import Network.FruitTart.Templates.Imports
import Network.FruitTart.Templates.Semantics
import Network.FruitTart.Templates.Types
import Network.FruitTart.Templates.View.Templates hiding (functionTable)
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [], [], toDyn index),
                       ("view", "GET", [IDParameter], [], toDyn view),
                       ("create", "GET", [], [], toDyn createGET),
                       ("create", "POST", [], [], toDyn createPOST),
                       ("edit", "POST", [IDParameter], [], toDyn edit),
                       ("delete", "GET", [IDParameter], [], toDyn deleteGET),
                       ("delete", "POST", [IDParameter], [], toDyn deletePOST)]


functionTable :: FunctionTable
functionTable
    = makeFunctionTable []


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
  bindQueryMultipleRows "Templates.Controller.Templates"
                        "rows"
                        [("templateID", TInt),
                         ("moduleName", TString),
                         ("templateName", TString)]
                        "SELECT id, module, name FROM templates ORDER BY module, name"
                        []
  pageContent <- getTemplate "Templates.Controller.Templates" "index"
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
  pageHeadItems <- getPageHeadItems
  navigationBar <- getNavigationBar currentPage
  loginButton <- getLoginButton currentPage
  popupMessage <- getPopupMessage
  templateTypePopup <- getTypePopup Content 0
  bodyRows
      <- (mapM (\((itemType, body), index) -> do
                 typePopup <- getTypePopup itemType index
                 return $  "<tr><td>"
                        ++ (if index /= 1
                              then "<div class=\"template-button up\">▲</div>"
                              else "")
                        ++ (if (index /= 1)
                               && (index /= (fromIntegral $ length bodies))
                              then "<br />"
                              else "")
                        ++ (if index /= (fromIntegral $ length bodies)
                              then "<div class=\"template-button down\">▼</div>"
                              else "")
                        ++ "</td><td>"
                        ++ "<div class=\"template-button add\"><b>+</b></div>"
                        ++ (if length bodies > 1
                              then "<div class=\"template-button remove\"><b>-</b></div>"
                              else "")
                        ++ "</td><td>"
                        ++ typePopup
                        ++ "</td><td>"
                        ++ "<textarea class=\"code autosizing\" "
                        ++ "name=\"body" ++ (show index) ++ "\" "
                        ++ "rows=\"" ++ (show $ rowCount body) ++ "\" cols=\"60\">"
                        ++ (escapeHTML body)
                        ++ "</textarea></td></tr>\n")
               $ zip bodies [1..])
         >>= return . concat
  output  $ "<html><head>\n"
         ++ "<title>" ++ moduleName ++ "." ++ templateName ++ "</title>\n"
         ++ pageHeadItems
         ++ "<link href=\"/css/templates.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "<script src=\"/js/templates.js\" type=\"text/ecmascript\"></script>\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ navigationBar
         ++ loginButton
         ++ popupMessage
         ++ "<noscript>\n"
         ++ "<h1>Javascript Required</h1>\n"
         ++ "<p>To use this page, you must have Javascript support in your browser, "
         ++ "and it must be enabled.</p>\n"
         ++ "</noscript>\n"
         ++ "<div id=\"noscript\" style=\"display: none;\">\n"
         ++ "<div id=\"templates\" style=\"display: none;\">\n"
         ++ "<table><tr id=\"template-row\"><td></td><td></td><td>"
         ++ templateTypePopup
         ++ "</td><td><textarea class=\"code autosizing\" name=\"body0\" "
         ++ "rows=\"1\" cols=\"60\"></textarea></td></tr></table>\n"
         ++ "<div id=\"template-button-up\" "
         ++ "class=\"template-button up\">▲</div>\n"
         ++ "<div id=\"template-button-down\" "
         ++ "class=\"template-button down\">▼</div>"
         ++ "<div id=\"template-button-add\" "
         ++ "class=\"template-button add\"><b>+</b></div>"
         ++ "<div id=\"template-button-remove\" "
         ++ "class=\"template-button remove\"><b>-</b></div>"
         ++ "</div>\n"
         ++ "<div class=\"form\">\n"
         ++ "<h1>" ++ moduleName ++ "." ++ templateName ++ "</h1>\n"
         ++ "<form method=\"POST\" action=\""
         ++ targetPage
         ++ "\">\n"
         ++ case maybeWarning of
              Just warning -> "<div class=\"warning note\">" ++ (escapeHTML warning)
                              ++ "</div>\n"
              Nothing -> ""
         ++ "<div>\n"
         ++ "<b>Module:</b> "
         ++ "<input type=\"text\" size=\"25\" name=\"module\" value=\""
         ++ (escapeAttribute moduleName)
         ++ "\"/>\n"
         ++ "<b>Name:</b> "
         ++ "<input type=\"text\" size=\"25\" name=\"name\" value=\""
         ++ (escapeAttribute templateName)
         ++ "\"/>\n"
         ++ "</div>\n"
         ++ "<table class=\"layout template-editor\">\n"
         ++ bodyRows
         ++ "<tr><td></td><td>"
         ++ "<div class=\"template-button add\"><b>+</b></div>"
         ++ "</td><td></td></tr>\n"
         ++ "</table>\n"
         ++ "<div class=\"submit\">"
         ++ "<button type=\"submit\" value=\"Save\">Save</button>"
         ++ "</div>\n"
         ++ "</form>\n"
         ++ "</div>\n"
         ++ "</div>\n"
         ++ (case maybeTemplateID of
               Just templateID ->
                    "<h1>Actions</h1>\n"
                    ++ "<ul>\n"
                    ++ "<li><a href=\"/templates/delete/" ++ (show templateID)
                    ++ "/\">Delete template</a></li>\n"
                    ++ "</ul>\n"
               Nothing -> "")
         ++ "</body></html>"


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
  bindQuery "Templates.Controller.Templates"
            [("moduleName", TString),
             ("templateName", TString)]
            "SELECT module, name FROM templates WHERE id = ?"
            [SQLInteger templateID]
  bind "Templates.Controller.Templates" "targetPage" targetPage
  pageContent <- getTemplate "Templates.Controller.Templates" "delete"
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


getTypePopup :: TemplateItemType -> Int64 -> FruitTart String
getTypePopup itemType index = do
  return $  "<select name=\"type" ++ (show index) ++ "\">"
         ++ "<option "
         ++ (if itemType == Content
               then "selected "
               else "")
         ++ "value=\"content\">Content</option>"
         ++ "<option "
         ++ (if itemType == Expression
               then "selected "
               else "")
         ++ "value=\"expression\">Expression</option>"
         ++ "</select>"


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


rowCount :: String -> Int
rowCount body = length $ split '\n' $ wordWrap body 60

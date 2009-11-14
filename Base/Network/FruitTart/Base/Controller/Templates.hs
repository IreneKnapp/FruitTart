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
  bindNamedQueryMultipleRows "Base.Controller.Templates" "indexRows" []
  pageContent <- getTemplate "Base.Controller.Templates" "index"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


view :: Int64 -> FruitTart CGIResult
view templateID = do
  let currentPage = "/templates/view/" ++ (show templateID) ++ "/"
      targetPage = "/templates/edit/" ++ (show templateID) ++ "/"
  maybeNames
      <- namedQuery "Base.Controller.Templates" "templateName" [SQLInteger templateID]
  case maybeNames of
    [[SQLText moduleName, SQLText templateName]] -> do
      sqlItems <- namedQuery "Base.Controller.Templates" "items" [SQLInteger templateID]
      let items = map (\[SQLText itemType, SQLText body] ->
                        (itemType, body))
                      sqlItems
      outputTemplatePage currentPage targetPage Nothing (Just templateID)
                         moduleName templateName items
    [] -> errorInvalidID "template"


copy :: Int64 -> FruitTart CGIResult
copy templateID = do
  let currentPage = "/templates/copy/" ++ (show templateID) ++ "/"
      targetPage = "/templates/create/"
  maybeNames
      <- namedQuery "Base.Controller.Templates" "templateName" [SQLInteger templateID]
  case maybeNames of
    [[SQLText moduleName, SQLText templateName]] -> do
      sqlItems <- namedQuery "Base.Controller.Templates" "items" [SQLInteger templateID]
      let items = map (\[SQLText itemType, SQLText body] ->
                        (itemType, body))
                      sqlItems
      outputTemplatePage currentPage targetPage Nothing Nothing
                         moduleName templateName items
    [] -> errorInvalidID "template"


createGET :: FruitTart CGIResult
createGET = do
  let currentPage = "/templates/create/"
      targetPage = "/templates/create/"
  outputTemplatePage currentPage targetPage Nothing Nothing
                     "Module" "template" [("content", "")]


outputTemplatePage
    :: String -> String -> (Maybe String) -> (Maybe Int64) -> String -> String
    -> [(String, String)]
    -> FruitTart CGIResult
outputTemplatePage currentPage targetPage maybeWarning maybeTemplateID
                   moduleName templateName bodies = do
  bind "Templates" "pageTitle" $ moduleName ++ "." ++ templateName
  pageHeadItems <- getPageHeadItems
  bind "Templates" "pageHeadItems" $ pageHeadItems
         ++ "<link href=\"/css/base.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
         ++ "<script src=\"/js/templates.js\" type=\"text/ecmascript\"></script>\n"
  navigationBar <- getNavigationBar currentPage
  bind "Templates" "navigationBar" navigationBar
  loginButton <- getLoginButton currentPage
  bind "Templates" "loginButton" loginButton
  popupMessage <- getPopupMessage
  bind "Templates" "popupMessage" popupMessage
  bind "Base.Controller.Templates" "bodies"
       $ map (\((itemType, body), index)
                  -> Map.fromList [(("Base.Controller.Templates", "itemType"),
                                    TemplateString itemType),
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
  namedQuery "Queries" "beginExclusiveTransaction" []
  [[SQLInteger count]]
      <- namedQuery "Base.Controller.Templates" "templateExists"
                    [SQLText moduleName, SQLText templateName]
  case count of
    0 -> do
      namedQuery "Base.Controller.Templates" "insertTemplate"
                 [SQLText moduleName, SQLText templateName]
      [[SQLInteger templateID]]
          <- namedQuery "Base.Controller.Templates" "templateJustInserted" []
      mapM (\((itemType, body), index) -> do
              namedQuery "Base.Controller.Templates" "insertTemplateItem"
                         [SQLInteger templateID,
                          SQLInteger index,
                          SQLText itemType,
                          SQLText body])
           $ zip items [0..]
      namedQuery "Queries" "commit" []
      setPopupMessage $ Just "Template created."
      seeOtherRedirect $ "/templates/index/"
    _ -> do
      namedQuery "Queries" "rollback" []
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
  namedQuery "Queries" "beginTransaction" []
  [[SQLInteger count]]
      <- namedQuery "Base.Controller.Templates" "templateExistsWithDifferentID"
                    [SQLText moduleName, SQLText templateName, SQLInteger templateID]
  case count of
    0 -> do
      namedQuery "Base.Controller.Templates" "updateTemplate" []
      namedQuery "Base.Controller.Templates" "deleteTemplateItems"
                 [SQLInteger templateID]
      mapM (\((itemType, body), index) -> do
              namedQuery "Base.Controller.Templates" "insertTemplateItem"
                         [SQLInteger templateID,
                          SQLInteger index,
                          SQLText itemType,
                          SQLText body])
           $ zip items [0..]
      namedQuery "Queries" "commit" []
      setPopupMessage $ Just "Template changed."
      seeOtherRedirect $ "/templates/index/"
    _ -> do
      namedQuery "Queries" "rollback" []
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
  bindNamedQuery "Base.Controller.Templates" "templateName" [SQLInteger templateID]
  bind "Base.Controller.Templates" "targetPage" targetPage
  pageContent <- getTemplate "Base.Controller.Templates" "delete"
  bind "Templates" "pageContent" pageContent
  page <- getTemplate "Templates" "page"
  output page


deletePOST :: Int64 -> FruitTart CGIResult
deletePOST templateID = do
  namedQuery "Queries" "beginTransaction" []
  namedQuery "Base.Controller.Templates" "deleteTemplate" [SQLInteger templateID]
  namedQuery "Base.Controller.Templates" "deleteTemplateItems" [SQLInteger templateID]
  namedQuery "Queries" "commit" []
  setPopupMessage $ Just "Template deleted."
  seeOtherRedirect "/templates/index/"


getInputItems :: FruitTart [(String, String)]
getInputItems
    = let getInputItem index = do
            maybeItemType <- getInput $ "type" ++ (show index)
            case maybeItemType of
              Nothing -> return Nothing
              Just itemType -> do
                maybeBody <- getInput $ "body" ++ (show index)
                body <- case maybeBody of
                          Nothing -> return ""
                          Just body -> return $ fromCRLF body
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

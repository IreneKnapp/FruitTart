module Network.FruitTart.View.Navigation (functionTable,
                                          getDefaultPage,
                                          getNavigationBar)
    where

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


functionTable :: FunctionTable
functionTable
    = makeFunctionTable [("getDefaultPage", toDyn getDefaultPage),
                         ("getNavigationBar", toDyn getNavigationBar)]


getDefaultPage :: FruitTart String
getDefaultPage = do
  [[SQLText defaultPage]] <- query "SELECT default_page FROM settings LIMIT 1" []
  return defaultPage


getNavigationBar :: String -> FruitTart String
getNavigationBar currentPage = do
  items <- query ("SELECT name, link, separator, always_enabled, class "
                  ++ "FROM navigation_items ORDER BY id")
                 []
  let item name link separator alwaysEnabled maybeClass =
          let class' = case maybeClass of
                         Nothing -> ""
                         Just class' -> " class=\"" ++ class' ++ "\""
          in if separator
             then "<div " ++ class' ++ "></div>"
             else if (link /= currentPage) || alwaysEnabled
                  then "<a href=\"" ++ (escapeAttribute link) ++ "\"" ++ class' ++ ">"
                       ++ (escapeHTML name) ++ "</a>"
                  else "<b" ++ class' ++ ">" ++ (escapeHTML name) ++ "</b>"
  result <- return $ "<div id=\"navigation\">"
           ++ (concat $ map (\[SQLText name,
                               SQLText link,
                               SQLInteger separator,
                               SQLInteger alwaysEnabled,
                               maybeClass]
                             -> item name
                                     link
                                     (separator /= 0)
                                     (alwaysEnabled /= 0)
                                     (case maybeClass of
                                        SQLNull -> Nothing
                                        SQLText class' -> Just class'))
                             items)
           ++ "</div>\n"
  return result

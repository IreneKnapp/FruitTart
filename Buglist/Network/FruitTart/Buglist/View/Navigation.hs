module Network.FruitTart.Buglist.View.Navigation (functionTable,
                                                  getSubnavigationBar)
    where

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


functionTable :: FunctionTable
functionTable
    = makeFunctionTable [("subnavigationBar", toDyn getSubnavigationBar)]


getSubnavigationBar :: String -> [Maybe (String, String)] -> FruitTart String
getSubnavigationBar currentPage items = do
  let item name link =
          if (link /= currentPage)
             then "<a href=\"" ++ (escapeAttribute link) ++ "\">"
                  ++ (escapeHTML name) ++ "</a>"
             else "<b>" ++ (escapeHTML name) ++ "</b>"
  return $ "<div class=\"navigation\">"
         ++ (concat $ map (\maybeItem -> case maybeItem of
                                           Just (name, link) -> item name link
                                           Nothing -> "<div class=\"separator\"></div>")
                          items)
         ++ "</div>\n"

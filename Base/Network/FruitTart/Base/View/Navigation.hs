module Network.FruitTart.Base.View.Navigation (getNavigationBar)
    where

import Network.FruitTart.Util


getNavigationBar :: String -> FruitTart String
getNavigationBar currentPage = do
  items <- query ("SELECT name, link, separator, always_enabled, name_is_html, class "
                  ++ "FROM navigation_items ORDER BY id")
                 []
  let item name link separator alwaysEnabled nameIsHtml maybeClass =
          let class' = case maybeClass of
                         Nothing -> ""
                         Just class' -> " class=\"" ++ class' ++ "\""
              name' = if nameIsHtml
                        then name
                        else escapeHTML name
          in if separator
             then "<div " ++ class' ++ "></div>"
             else if (link /= currentPage) || alwaysEnabled
                  then "<a href=\"" ++ (escapeAttribute link) ++ "\"" ++ class' ++ ">"
                       ++ name' ++ "</a>"
                  else "<b" ++ class' ++ ">" ++ name' ++ "</b>"
  result <- return $ "<div id=\"navigation\">"
           ++ (concat $ map (\[SQLText name,
                               SQLText link,
                               SQLInteger separator,
                               SQLInteger alwaysEnabled,
                               SQLInteger nameIsHtml,
                               maybeClass]
                             -> item name
                                     link
                                     (separator /= 0)
                                     (alwaysEnabled /= 0)
                                     (nameIsHtml /= 0)
                                     (case maybeClass of
                                        SQLNull -> Nothing
                                        SQLText class' -> Just class'))
                             items)
           ++ "</div>\n"
  return result

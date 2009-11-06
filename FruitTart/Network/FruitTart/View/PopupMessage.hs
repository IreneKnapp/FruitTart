module Network.FruitTart.View.PopupMessage (functionTable,
                                            setPopupMessage,
                                            getPopupMessage) where

import Network.FruitTart.Base
import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


functionTable :: FunctionTable
functionTable
    = makeFunctionTable [("setPopupMessage", toDyn setPopupMessage),
                         ("getPopupMessage", toDyn getPopupMessage)]


setPopupMessage :: Maybe String -> FruitTart ()
setPopupMessage maybeMessage = do
  sessionID <- getSessionID
  query "UPDATE sessions SET popup_message = ? WHERE id = ?"
        [case maybeMessage of
           Nothing -> SQLNull
           Just message -> SQLText message,
         SQLInteger sessionID]
  return ()


getPopupMessage :: FruitTart String
getPopupMessage = do
  sessionID <- getSessionID
  [[maybeMessage]] <- query "SELECT popup_message FROM sessions WHERE id = ?"
                            [SQLInteger sessionID]
  query "UPDATE sessions SET popup_message = NULL WHERE ID = ?"
        [SQLInteger sessionID]
  case maybeMessage of
    SQLNull -> return ""
    SQLText message -> return $ "<div id=\"popupmessage\">"
                              ++ (escapeHTML message)
                              ++ "</div>\n"

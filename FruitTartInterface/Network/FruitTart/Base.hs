module Network.FruitTart.Base (
                               getBindingsMVar,
                               getCaptchaCacheMVar,
                               getInput,
                               permanentRedirect,
                               seeOtherRedirect,
                               error404,
                               error500,
                               errorControllerUndefined,
                               errorActionUndefined,
                               errorActionParameters,
                               errorActionMethod,
                               errorInvalidID,
                               parseURL,
                               canonicalURL,
                               getSessionID
                              )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.FastCGI

import Network.FruitTart.Util


getBindingsMVar :: FruitTart (MVar (Map (String, String) (TemplateValue a)))
  FruitTartState { bindingsMVar = bindingsMVar } <- get
  return bindingsMVar


getCaptchaCacheMVar :: FruitTart (MVar (Map Int64 (String, ByteString)))
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  return captchaCacheMVar


getInput :: String -> FruitTart (Maybe String)
getInput key = do
  state <- get
  formVariableMap <- liftIO $ readMVar $ formVariableMapMVar state
  return $ Map.lookup key formVariableMap


error404 :: String -> FruitTart ()
error404 text = do
  setResponseStatus 404
  setResponseHeader HttpContentType "text/html; charset=UTF8"
  fPutStr $ "<html><head><title>404 Not Found</title></head>"
            ++ "<body><h1>404 Not Found</h1><p>FruitTart encountered an error while "
            ++ "processing this request: " ++ text ++ "</p></body></html>"


error500 :: FruitTart ()
error500 = do
  setResponseStatus 500
  setResponseHeader HttpContentType "text/html; charset=UTF8"
  fPutStr $ "<html><head><title>500 Internal Server Error</title></head>"
            ++ "<body><h1>500 Internal Server Error</h1><p>"
            ++ "FruitTart encountered an error while "
            ++ "processing this request.  The logfile has more details.</p></body></html>"


errorControllerUndefined :: String -> FruitTart ()
errorControllerUndefined controllerName =
    error404 $ "No controller named " ++ controllerName ++ " is defined."


errorActionUndefined :: String -> String -> FruitTart ()
errorActionUndefined controllerName actionName =
    error404 $ "No action named named " ++ actionName ++ " is defined "
             ++ "for the controller " ++ controllerName ++ "."


errorActionParameters :: String -> String -> FruitTart ()
errorActionParameters controllerName actionName =
    error404 $ "Invalid parameters to the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."


errorActionMethod :: String -> String -> String -> FruitTart ()
errorActionMethod controllerName actionName method =
    error404 $ "Invalid HTTP method " ++ method ++ " for the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."


errorInvalidID :: String -> FruitTart ()
errorInvalidID kind =
    error404 $ "No " ++ kind ++ " by that ID exists." 


defaultController :: String
defaultController = "issues"


defaultAction :: String
defaultAction = "index"


parseURL :: String -> (String, String, [String], [(String, String)])
parseURL url =
    let pathComponents = if (length url >= 1) && (head url) == '/'
                         then pathComponents' $ tail url
                         else error "URL does not start with slash."
        pathComponents' "" = []
        pathComponents' string = case break ((==) '/') string of
                                   (start, "") -> [start]
                                   (start, _:rest) -> start : pathComponents' rest
        controller = if length pathComponents >= 1
                     then map toLower $ pathComponents !! 0
                     else defaultController
        action = if length pathComponents >= 2
                 then map toLower $ pathComponents !! 1
                 else defaultAction
        parameters = if length pathComponents >= 2
                     then tail $ tail pathComponents
                     else []
        urlParameters = filter ((/=) "")
                        $ filter (notElem ':') parameters
        namedParameters = map (\parameter -> case break ((==) ':') parameter of
                                               (name, _:value)
                                                   -> (map toLower name, value))
                          $ filter (elem ':') parameters
    in (controller, action, urlParameters, namedParameters)


canonicalURL :: String -> String -> [String] -> [(String, String)] -> String
canonicalURL controllerName actionName urlParameters namedParameters =
    let basePart = "/"
                   ++ (map toLower controllerName) ++ "/"
                   ++ (map toLower actionName) ++ "/"
        urlParametersPart = concat $ map (\parameter -> concat [parameter, "/"])
                            urlParameters
        namedParametersPart = concat $ map (\(key, value) ->
                                                concat [map toLower key,
                                                        ":",
                                                        value,
                                                        "/"])
                              namedParameters
    in concat [basePart, urlParametersPart, namedParametersPart]


getSessionID :: FruitTart Int64
getSessionID = do
  FruitTartState { sessionID = sessionID } <- get
  return $ fromJust sessionID

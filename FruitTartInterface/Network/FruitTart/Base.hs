module Network.FruitTart.Base (
                               getInput,
                               output,
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

import Control.Monad.State
import Data.Char
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe
import Network.FastCGI hiding (getInput, output, logCGI)

import Network.FruitTart.Util


getInput :: String -> FruitTart (Maybe String)
getInput key = do
  maybeValue <- getInputFPS $ key
  return $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ UTF8.toString value


output :: String -> FruitTart CGIResult
output string = outputFPS $ UTF8.fromString string


permanentRedirect :: String -> FruitTart CGIResult
permanentRedirect url = do
  setStatus 301 "Moved Permanently"
  setHeader "Location" url
  output ""


seeOtherRedirect :: String -> FruitTart CGIResult
seeOtherRedirect url = do
  setStatus 303 "See Other"
  setHeader "Location" url
  output ""


error404 :: String -> FruitTart CGIResult
error404 text = do
  setStatus 404 "Not Found"
  setHeader "Content-Type" "text/html; charset=UTF8"
  output $ "<html><head><title>404 Not Found</title></head>"
         ++ "<body><h1>404 Not Found</h1><p>FruitTart encountered an error while "
         ++ "processing this request: " ++ text ++ "</p></body></html>"


error500 :: FruitTart CGIResult
error500 = do
  setStatus 500 "Internal Server Error"
  setHeader "Content-Type" "text/html; charset=UTF8"
  output $ "<html><head><title>500 Internal Server Error</title></head>"
         ++ "<body><h1>500 Internal Server Error</h1><p>"
         ++ "FruitTart encountered an error while "
         ++ "processing this request.  The logfile has more details.</p></body></html>"


errorControllerUndefined :: String -> FruitTart CGIResult
errorControllerUndefined controllerName =
    error404 $ "No controller named " ++ controllerName ++ " is defined."


errorActionUndefined :: String -> String -> FruitTart CGIResult
errorActionUndefined controllerName actionName =
    error404 $ "No action named named " ++ actionName ++ " is defined "
             ++ "for the controller " ++ controllerName ++ "."


errorActionParameters :: String -> String -> FruitTart CGIResult
errorActionParameters controllerName actionName =
    error404 $ "Invalid parameters to the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."


errorActionMethod :: String -> String -> String -> FruitTart CGIResult
errorActionMethod controllerName actionName method =
    error404 $ "Invalid HTTP method " ++ method ++ " for the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."


errorInvalidID :: String -> FruitTart CGIResult
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
        urlParameters = map (map toLower)
                        $ filter ((/=) "")
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
        urlParametersPart = concat $ map (\parameter ->
                                              concat [map toLower parameter, "/"])
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

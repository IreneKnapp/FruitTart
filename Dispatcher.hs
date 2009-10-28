module Dispatcher (
       		   CGI,
		   CGIResult,
		   
                   main,
                   processRequest,
                   output,
                   permanentRedirect,
                   error404,
                   errorControllerUndefined,
                   errorActionUndefined,
                   errorActionParameters,
                   errorTemplateUndefined,
                   parseURL,
                   canonicalURL
                  )
    where

import Control.Concurrent
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Network.FastCGI hiding (output)
import Prelude hiding (catch)

import qualified Controller.Tickets


main :: IO ()
main = runFastCGI processRequest

processRequest :: CGI CGIResult
processRequest = do
    setHeader "Content-Type" "text/html; charset=UTF8"
    url <- queryString
    (controllerName, actionName, urlParameters, namedParameters)
        <- return $ parseURL url
    canonical <- return
        $ canonicalURL controllerName actionName urlParameters namedParameters
    if url /= canonical
      then permanentRedirect canonical
      else case controllerName of
             name | name == "tickets" ->
                      case actionName of
                        name | name == "index" -> Controller.Tickets.index
                                                  urlParameters namedParameters
                             | name == "view" -> Controller.Tickets.view
                                                 urlParameters namedParameters
                             | True -> errorActionUndefined controllerName actionName
                  | True -> errorControllerUndefined controllerName

output :: String -> CGI CGIResult
output string = outputFPS $ UTF8.fromString string

permanentRedirect :: String -> CGI CGIResult
permanentRedirect url = do
  setStatus 301 "Moved Permanently"
  setHeader "Location" url
  output ""

error404 :: String -> CGI CGIResult
error404 text = do
  setStatus 404 "Not Found"
  setHeader "Content-Type" "text/html; charset=UTF8"
  output $ "<html><head><title>404 Not Found</title></head>"
         ++ "<body><h1>404 Not Found</h1><p>Buglist encountered an error while "
         ++ "processing this request: " ++ text ++ "</p></body></html>"

errorControllerUndefined :: String -> CGI CGIResult
errorControllerUndefined controllerName =
    error404 $ "No controller named " ++ controllerName ++ " is defined."

errorActionUndefined :: String -> String -> CGI CGIResult
errorActionUndefined controllerName actionName =
    error404 $ "No action named named " ++ actionName ++ " is defined "
             ++ "for the controller " ++ controllerName ++ "."

errorActionParameters :: String -> String -> CGI CGIResult
errorActionParameters controllerName actionName =
    error404 $ "Invalid number of parameters to the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."

errorTemplateUndefined :: String -> CGI CGIResult
errorTemplateUndefined templateName =
    error404 $ "No template named " ++ templateName ++ " is defined."

defaultController :: String
defaultController = "tickets"

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

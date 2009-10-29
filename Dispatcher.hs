{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Dispatcher (
       		   Buglist,
		   CGIResult,
		   
                   processRequest,
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
                   canonicalURL
                  )
    where

import Control.Concurrent
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.Int
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Network.FastCGI hiding (output, logCGI)
import Prelude hiding (catch)

import qualified Controller.Issues
import qualified Controller.Users
import Log
import Types


dispatchTable :: DispatchTable
dispatchTable
    = Map.fromList
      [("issues",
        Map.fromList [("index",
                       Map.fromList [("GET", ([],
                                              toDyn Controller.Issues.index))]),
                      ("view",
                       Map.fromList [("GET", ([IDParameter],
                                              toDyn Controller.Issues.view))]),
                      ("create",
                       Map.fromList [("GET", ([],
                                              toDyn Controller.Issues.createGET)),
                                     ("POST", ([],
                                               toDyn Controller.Issues.createPOST))]),
                      ("comment",
                       Map.fromList [("POST", ([IDParameter],
                                               toDyn Controller.Issues.comment))])]),
       ("users",
        Map.fromList [("index",
                       Map.fromList [("GET", ([],
                                              toDyn Controller.Users.index))]),
                      ("view",
                       Map.fromList [("GET", ([IDParameter],
                                              toDyn Controller.Users.view))])])]


instance Typeable (Buglist CGIResult)
    where typeOf function = mkTyConApp (mkTyCon "Buglist CGIResult") []


processRequest :: Buglist CGIResult
processRequest = do
  buglistState <- get
  liftCGI $ catchCGI (evalStateT processRequest' buglistState)
                     (\e -> evalStateT (do
                                         logCGI $ "Buglist: " ++ show e
                                         error500)
                                       buglistState)
                              

processRequest' :: Buglist CGIResult
processRequest' = do
    setHeader "Content-Type" "text/html; charset=UTF8"
    url <- queryString
    (controllerName, actionName, urlParameters, namedParameters)
        <- return $ parseURL url
    canonical <- return
        $ canonicalURL controllerName actionName urlParameters namedParameters
    method <- requestMethod
    if url /= canonical
      then permanentRedirect canonical
      else do
        maybeControllerData <- return $ Map.lookup controllerName dispatchTable
        case maybeControllerData of
          Nothing -> errorControllerUndefined controllerName
          Just controllerData
            -> do
             maybeActionData <- return $ Map.lookup actionName controllerData
             case maybeActionData of
               Nothing -> errorActionUndefined controllerName actionName
               Just actionData
                   -> do
                    maybeMethodData <- return $ Map.lookup method actionData
                    case maybeMethodData of
                      Nothing -> errorActionMethod controllerName actionName method
                      Just (urlParameterTypes, dynamicFunction)
                          -> do
                           case urlParameterTypes of
                             [IDParameter] -> do
                                             case (urlParameters, namedParameters) of
                                               ([id], [])
                                                   | length id > 0 && all isDigit id ->
                                                       (fromJust
                                                        $ fromDynamic dynamicFunction) 
                                                       (read id :: Int64)
                                               _ -> errorActionParameters controllerName
                                                                          actionName
                             _ -> do
                                   case (urlParameters, namedParameters) of
                                     ([], []) -> (fromJust $ fromDynamic dynamicFunction)
                                     _ -> errorActionParameters controllerName actionName

{-
      else case controllerName of
             name | name == "issues" ->
                      case actionName of
                        name | name == "index" ->
                                 case method of
                                   _ | method == "GET" ->
                                         case (urlParameters, namedParameters) of
                                           ([], []) -> Controller.Issues.index
                                           _ -> errorActionParameters controllerName
                                                                      actionName
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                             | name == "view" ->
                                 case method of
                                   _ | method == "GET" ->
                                         case (urlParameters, namedParameters) of
                                           ([id], []) | length id > 0 && all isDigit id
                                            -> Controller.Issues.view (read id)
                                           _ -> errorActionParameters controllerName
                                                                      actionName
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                             | name == "create" ->
                                 case method of
                                   _ | method == "POST" ->
                                         case (urlParameters, namedParameters) of
                                           ([], []) -> Controller.Issues.createPOST
                                     | method == "GET" ->
                                         case (urlParameters, namedParameters) of
                                           ([], []) -> Controller.Issues.createGET
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                                   _ -> errorActionParameters controllerName actionName
                             | name == "comment" ->
                                 case method of
                                   _ | method == "POST" ->
                                         case (urlParameters, namedParameters) of
                                           ([id], []) | length id > 0 && all isDigit id
                                            -> Controller.Issues.comment (read id)
                                           _ -> errorActionParameters controllerName
                                                                      actionName
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                             | True -> errorActionUndefined controllerName actionName
             name | name == "users" ->
                      case actionName of
                        name | name == "index" ->
                                 case method of
                                   _ | method == "GET" ->
                                         case (urlParameters, namedParameters) of
                                           ([], []) -> Controller.Users.index
                                           _ -> errorActionParameters controllerName
                                                                      actionName
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                             | name == "view" ->
                                 case method of
                                   _ | method == "GET" ->
                                         case (urlParameters, namedParameters) of
                                           ([id], []) | length id > 0 && all isDigit id
                                              -> Controller.Users.view (read id)
                                           _ -> errorActionParameters controllerName
                                                                      actionName
                                     | True -> errorActionMethod controllerName
                                                                 actionName
                                                                 method
                             | True -> errorActionUndefined controllerName actionName
                  | True -> errorControllerUndefined controllerName
-}

output :: String -> Buglist CGIResult
output string = outputFPS $ UTF8.fromString string

permanentRedirect :: String -> Buglist CGIResult
permanentRedirect url = do
  setStatus 301 "Moved Permanently"
  setHeader "Location" url
  output ""

seeOtherRedirect :: String -> Buglist CGIResult
seeOtherRedirect url = do
  setStatus 303 "See Other"
  setHeader "Location" url
  output ""

error404 :: String -> Buglist CGIResult
error404 text = do
  setStatus 404 "Not Found"
  setHeader "Content-Type" "text/html; charset=UTF8"
  output $ "<html><head><title>404 Not Found</title></head>"
         ++ "<body><h1>404 Not Found</h1><p>Buglist encountered an error while "
         ++ "processing this request: " ++ text ++ "</p></body></html>"

error500 :: Buglist CGIResult
error500 = do
  setStatus 500 "Internal Server Error"
  setHeader "Content-Type" "text/html; charset=UTF8"
  output $ "<html><head><title>500 Internal Server Error</title></head>"
         ++ "<body><h1>500 Internal Server Error</h1><p>"
         ++ "Buglist encountered an error while "
         ++ "processing this request.  The logfile has more details.</p></body></html>"

errorControllerUndefined :: String -> Buglist CGIResult
errorControllerUndefined controllerName =
    error404 $ "No controller named " ++ controllerName ++ " is defined."

errorActionUndefined :: String -> String -> Buglist CGIResult
errorActionUndefined controllerName actionName =
    error404 $ "No action named named " ++ actionName ++ " is defined "
             ++ "for the controller " ++ controllerName ++ "."

errorActionParameters :: String -> String -> Buglist CGIResult
errorActionParameters controllerName actionName =
    error404 $ "Invalid number of parameters to the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."

errorActionMethod :: String -> String -> String -> Buglist CGIResult
errorActionMethod controllerName actionName method =
    error404 $ "Invalid HTTP method " ++ method ++ " for the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."

errorInvalidID :: String -> Buglist CGIResult
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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Dispatcher (
       		   FruitTart,
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
                   canonicalURL,
                   getSessionID
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
import Random
import System.Time

import Database
import Log
import SQLite3 (SQLData(..))
import qualified SQLite3 as SQL
import Types


instance Typeable (FruitTart CGIResult)
    where typeOf function = mkTyConApp (mkTyCon "FruitTart CGIResult") []


processRequest :: ControllerTable -> FruitTart CGIResult
processRequest dispatchTable  = do
  fruitTartState <- get
  liftCGI $ catchCGI (evalStateT (processRequest' dispatchTable) fruitTartState)
                     (\e -> evalStateT (do
                                         logCGI $ "FruitTart: " ++ show e
                                         error500)
                                       fruitTartState)


processRequest' :: ControllerTable -> FruitTart CGIResult
processRequest' dispatchTable = do
  initializeSession
  setHeader "Content-Type" "text/html; charset=UTF8"
  url <- queryString
  (controllerName, actionName, urlParameters, namedParameters)
        <- return $ parseURL url
  canonical <- return
        $ canonicalURL controllerName actionName urlParameters namedParameters
  method <- requestMethod
  result <- if url /= canonical
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
                      Just (urlParameterTypes, namedParameterTypes, dynamicFunction)
                          -> invokeDynamicFunction controllerName actionName
                                                   dynamicFunction
                                                   urlParameters urlParameterTypes
                                                   namedParameters namedParameterTypes
  endSession
  return result

invokeDynamicFunction
    :: String -> String
    -> Dynamic
    -> [String] -> [ParameterType]
    -> [(String, String)] -> [(String, ParameterType)]
    -> FruitTart CGIResult
invokeDynamicFunction controllerName actionName
                      dynamicFunction
                      urlParameters urlParameterTypes
                      namedParameters namedParameterTypes
    = let handleURLParameters urlParameters urlParameterTypes actualParameters =
              case urlParameterTypes of
                [] -> case urlParameters of
                        [] -> return $ Right $ reverse actualParameters
                        _ -> do
                          result <- errorActionParameters controllerName actionName
                          return $ Left $ result
                (parameterType:restURLParameterTypes)
                    -> case urlParameters of
                         (parameter:restURLParameters)
                             | validateParameter parameterType parameter
                             -> handleURLParameters restURLParameters
                                                    restURLParameterTypes
                                                    (readParameter parameterType
                                                                   parameter
                                                     : actualParameters)
                         _ -> do
                           result <- errorActionParameters controllerName actionName
                           return $ Left $ result
          handleNamedParameters namedParameters namedParameterTypes actualParameters =
              case namedParameterTypes of
                [] -> case namedParameters of
                        [] -> return $ Right $ reverse actualParameters
                        _ -> do
                          result <- errorActionParameters controllerName actionName
                          return $ Left $ result
                ((formalParameterName, parameterType):restNamedParameterTypes)
                    -> case namedParameters of
                         ((actualParameterName, parameter):restNamedParameters)
                             | (formalParameterName == actualParameterName)
                               && (validateParameter parameterType parameter)
                             -> handleNamedParameters restNamedParameters
                                                      restNamedParameterTypes
                                                      ((dynamicJust
                                                        parameterType
                                                        $ readParameter parameterType
                                                                        parameter)
                                                       : actualParameters)
                         ((actualParameterName, parameter):restNamedParameters)
                             | (formalParameterName == actualParameterName)
                             -> do
                               result <- errorActionParameters controllerName actionName
                               return $ Left $ result
                         _ -> handleNamedParameters namedParameters
                                                    restNamedParameterTypes
                                                    (dynamicNothing parameterType
                                                     : actualParameters)
          dynApplyAll dynamicFunction [] = dynamicFunction
          dynApplyAll dynamicFunction (parameter:rest)
              = dynApplyAll (dynApp dynamicFunction parameter) rest in
      do
        eitherErrorActualURLParameters
            <- handleURLParameters urlParameters urlParameterTypes []
        case eitherErrorActualURLParameters of
          Left error -> return error
          Right actualURLParameters -> do
            eitherErrorActualNamedParameters
                <- handleNamedParameters namedParameters namedParameterTypes []
            case eitherErrorActualNamedParameters of
              Left error -> return error
              Right actualNamedParameters -> do
                fromJust
                $ fromDynamic
                  $ dynApplyAll dynamicFunction
                                (concat [actualURLParameters, actualNamedParameters])


validateParameter :: ParameterType -> String -> Bool
validateParameter IDParameter id = length id > 0 && all isDigit id
validateParameter StringParameter _ = True
validateParameter EitherStringIDParameter _ = True


readParameter :: ParameterType -> String -> Dynamic
readParameter IDParameter id = toDyn (read id :: Int64)
readParameter StringParameter string = toDyn string
readParameter EitherStringIDParameter string
    = toDyn $ if length string > 0 && all isDigit string
              then Right $ (read string :: Int64)
              else Left string


dynamicJust :: ParameterType -> Dynamic -> Dynamic
dynamicJust IDParameter item = toDyn (fromDynamic item :: Maybe Int64)
dynamicJust StringParameter item = toDyn (fromDynamic item :: Maybe String)
dynamicJust EitherStringIDParameter item
    = toDyn (fromDynamic item :: Maybe (Either String Int64))


dynamicNothing :: ParameterType -> Dynamic
dynamicNothing IDParameter = toDyn (Nothing :: Maybe Int64)
dynamicNothing StringParameter = toDyn (Nothing :: Maybe String)
dynamicNothing EitherStringIDParameter = toDyn (Nothing :: Maybe (Either String Int64))


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


initializeSession :: FruitTart ()
initializeSession = do
  timestamp <- getTimestamp
  query "DELETE FROM sessions WHERE timestamp_activity < ?"
        [SQLInteger $ timestamp - (60*60*24*30)]
  maybeSessionCookie <- getCookie "session"
  sessionID <- case maybeSessionCookie of
    Nothing -> generateSessionID
    Just sessionCookie -> do
                      rows <- query "SELECT id FROM sessions WHERE id = ?"
                              [SQLInteger $ read sessionCookie]
                      case rows of
                        [[SQLInteger _]] -> return $ read sessionCookie
                        [] -> generateSessionID
  query "UPDATE sessions SET timestamp_activity = ? WHERE id = ?"
        [SQLInteger timestamp,
         SQLInteger sessionID]
  setCookie $ Cookie {
                      cookieName = "session",
                      cookieValue = show sessionID,
                      cookiePath = Just "/",
                      cookieExpires = Just $ CalendarTime {
                                        ctYear = 3000,
                                        ctMonth = January,
                                        ctDay = 1,
                                        ctHour = 0,
                                        ctMin = 0,
                                        ctSec = 0,
                                        ctPicosec = 0,
                                        ctWDay = Monday,
                                        ctYDay = 1,
                                        ctTZName = "UTC",
                                        ctTZ = 0,
                                        ctIsDST = False
                                      },
                      cookieDomain = Nothing,
                      cookieSecure = False
                    }
  fruitTartState <- get
  put $ fruitTartState { sessionID = Just sessionID }


endSession :: FruitTart ()
endSession = do
  fruitTartState <- get
  put $ fruitTartState { sessionID = Nothing }


generateSessionID :: FruitTart Int64
generateSessionID = do
  sessionID <- liftIO $ randomRIO (0, fromIntegral (maxBound :: Int64) :: Integer)
               >>= return . fromIntegral
  timestamp <- getTimestamp
  query ("INSERT INTO sessions (id, timestamp_activity, logged_in_user) "
         ++ "VALUES (?, ?, NULL)")
        [SQLInteger sessionID, SQLInteger timestamp]
  return sessionID


getSessionID :: FruitTart Int64
getSessionID = do
  FruitTartState { sessionID = sessionID } <- get
  return $ fromJust sessionID

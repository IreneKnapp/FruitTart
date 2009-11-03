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

import qualified Controller.Captcha
import qualified Controller.Issues
import qualified Controller.Login
import qualified Controller.Users
import qualified Controller.Synchronization
import Database
import Log
import SQLite3 (SQLData(..))
import qualified SQLite3 as SQL
import Types


dispatchTable :: DispatchTable
dispatchTable
    = Map.fromList
      [("login",
        Map.fromList [("login",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Login.loginGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn Controller.Login.loginPOST))]),
                      ("logout",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Login.logout))]),
                      ("account",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Login.accountGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn Controller.Login.accountPOST))]),
                      ("password",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Login.passwordGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn Controller.Login.passwordPOST))])]),
       ("issues",
        Map.fromList [("index",
                       Map.fromList [("GET", ([],
                                              [("which", StringParameter),
                                               ("module", EitherStringIDParameter)],
                                              toDyn Controller.Issues.index))]),
                      ("view",
                       Map.fromList [("GET", ([IDParameter],
                                              [],
                                              toDyn Controller.Issues.view))]),
                      ("create",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Issues.createGET)),
                                     ("POST", ([],
                                               [],
                                               toDyn Controller.Issues.createPOST))]),
                      ("comment",
                       Map.fromList [("POST", ([IDParameter],
                                               [],
                                               toDyn Controller.Issues.comment))]),
                      ("edit",
                       Map.fromList [("POST", ([IDParameter],
                                               [],
                                               toDyn Controller.Issues.edit))])]),
       ("users",
        Map.fromList [("index",
                       Map.fromList [("GET", ([],
                                              [],
                                              toDyn Controller.Users.index))]),
                      ("view",
                       Map.fromList [("GET", ([IDParameter],
                                              [],
                                              toDyn Controller.Users.view))])]),
       ("captcha",
        Map.fromList [("index",
                       Map.fromList [("GET", ([IDParameter],
                                              [],
                                              toDyn Controller.Captcha.index))])]),
       ("synchronization",
        Map.fromList
               [("index",
                 Map.fromList [("GET", ([],
                                        [],
                                        toDyn Controller.Synchronization.index))]),
                ("issue",
                 Map.fromList [("GET", ([IDParameter],
                                        [],
                                        toDyn Controller.Synchronization.issueGET)),
                               ("POST", ([IDParameter],
                                         [],
                                         toDyn Controller.Synchronization.issuePOST))]),
                ("user-issue-change",
                 Map.fromList
                 [("GET", ([IDParameter, IDParameter, IDParameter],
                           [],
                           toDyn Controller.Synchronization.userIssueChangeGET)),
                  ("POST", ([IDParameter, IDParameter, IDParameter],
                            [],
                            toDyn Controller.Synchronization.userIssueChangePOST))]),
                ("user-issue-comment",
                 Map.fromList
                 [("GET", ([IDParameter, IDParameter, IDParameter],
                           [],
                           toDyn Controller.Synchronization.userIssueCommentGET)),
                  ("POST", ([IDParameter, IDParameter, IDParameter],
                            [],
                            toDyn Controller.Synchronization.userIssueCommentPOST))]),
                ("user-issue-attachment",
                 Map.fromList
                 [("GET", ([IDParameter, IDParameter, IDParameter],
                           [],
                           toDyn Controller.Synchronization.userIssueAttachmentGET)),
                  ("POST", ([IDParameter, IDParameter, IDParameter],
                            [],
                          toDyn Controller.Synchronization.userIssueAttachmentPOST))])])]


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
    -> Buglist CGIResult
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
    error404 $ "Invalid parameters to the action " ++ actionName
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


initializeSession :: Buglist ()
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
  buglistState <- get
  put $ buglistState { sessionID = Just sessionID }


endSession :: Buglist ()
endSession = do
  buglistState <- get
  put $ buglistState { sessionID = Nothing }


generateSessionID :: Buglist Int64
generateSessionID = do
  sessionID <- liftIO $ randomRIO (0, fromIntegral (maxBound :: Int64) :: Integer)
               >>= return . fromIntegral
  timestamp <- getTimestamp
  query ("INSERT INTO sessions (id, timestamp_activity, logged_in_user) "
         ++ "VALUES (?, ?, NULL)")
        [SQLInteger sessionID, SQLInteger timestamp]
  return sessionID


getSessionID :: Buglist Int64
getSessionID = do
  BuglistState { sessionID = sessionID } <- get
  return $ fromJust sessionID

module Network.FruitTart.Dispatcher (processRequest) where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Dynamic
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import Network.FastCGI
import Numeric
import Prelude hiding (catch)
import Random
import System.Time

import qualified Database.SQLite3 as SQL
import Network.FruitTart.Custard.Semantics
import Network.FruitTart.Types
import Network.FruitTart.Util


processRequest :: FruitTart ()
processRequest  = do
  fCatch processRequest'
         (\e -> do
                  fLog $ "FruitTart: " ++ show (e :: SomeException)
                  fCatch (do
                           applyFunctionGivenName ControllerContext
                                                  Map.empty
                                                  "Base"
                                                  "error500"
                                                  []
                           return ())
                         (\e -> do
                           return (e :: SomeException)
                           setResponseStatus 500
                           setResponseHeader HttpContentType
                                             "text/html; charset=UTF8"
                           fPutStr $ "<html><head><title>"
                                     ++ "500 Internal Server Error"
                                     ++ "</title></head><body>"
                                     ++ "<h1>500 Internal Server Error</h1><p>"
                                     ++ "FruitTart encountered an error while "
                                     ++ "processing this request.  The logfile "
                                     ++ "has more details.</p></body></html>"))


processRequest' :: FruitTart ()
processRequest' = do
  initializeSession
  formInput <- processFormInput
  setMimeType
  maybeURL <- getQueryString
  let url = fromJust maybeURL
      (controllerName, actionName, unnamedParameters, namedParameters) = parseURL url
      canonical = canonicalURL controllerName actionName
                               unnamedParameters namedParameters
  maybeMethod <- getRequestMethod
  let method = fromJust maybeMethod
  if url /= canonical
    then permanentRedirect canonical
    else do
      maybeControllerModuleName <- lookupController controllerName
      case maybeControllerModuleName of
        Nothing -> do
          applyFunctionGivenName ControllerContext
                                 Map.empty
                                 "Base"
                                 "errorControllerUndefined"
                                 [CustardString controllerName]
          return ()
        Just controllerModuleName
          -> do
           maybeActionIDAndFunctionName
             <- lookupAction controllerModuleName actionName method
           case maybeActionIDAndFunctionName of
             Nothing -> do
               applyFunctionGivenName ControllerContext
                                      Map.empty
                                      "Base"
                                      "errorActionUndefined"
                                      [CustardString controllerName,
                                       CustardString actionName,
                                       CustardString method]
               return ()
             Just (actionID, functionName) -> do
               (mandatoryParameterTypes,
                optionalParameterTypes,
                namedParameterTypes) <- lookupParameterTypes actionID
               parameters <- decodeParameters unnamedParameters
                                              namedParameters
                                              mandatoryParameterTypes
                                              optionalParameterTypes
                                              namedParameterTypes
               applyFunctionGivenName ControllerContext
                                      formInput
                                      controllerName
                                      functionName
                                      parameters
               return ()
  endSession


lookupController :: String -> FruitTart (Maybe String)
lookupController name = do
  return Nothing
  -- TODO


lookupAction :: String -> String -> String -> FruitTart (Maybe (Int64, String))
lookupAction moduleName actionName method = do
  return Nothing
  -- TODO


lookupParameterTypes :: Int64
                     -> FruitTart ([CustardValueType],
                                   [CustardValueType],
                                   Map String CustardValueType)
lookupParameterTypes actionID = do
  return ([], [], Map.empty)
  -- TODO


decodeParameters :: [String]
                 -> Map String String
                 -> [CustardValueType]
                 -> [CustardValueType]
                 -> Map String CustardValueType
                 -> FruitTart [CustardValue]
decodeParameters unnamedParameters
                 namedParameters
                 mandatoryParameterTypes
                 optionalParameterTypes
                 namedParameterTypes = do
  return []
  -- TODO


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


initializeSession :: FruitTart ()
initializeSession = do
  timestamp <- getTimestamp
  query "DELETE FROM sessions WHERE timestamp_activity < ?"
        [SQLInteger $ timestamp - (60*60*24*30)]
  maybeSessionCookie <- getCookieValue "session"
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
  setCookie $ mkCookie "session"
                       (show sessionID)
                       (Just "/")
                       Nothing
                       (Just 198743552)
                       False
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


processFormInput :: FruitTart (Map String String)
processFormInput = do
  state <- get
  maybeRequestMethod <- getRequestMethod
  case maybeRequestMethod of
    Just "POST" -> do
      maybeContentType <- getContentType
      case maybeContentType of
        Just "application/x-www-form-urlencoded" -> do
          inputData <- fGetContents
          return $ parseFormURLEncoded $ UTF8.toString inputData
        _ -> do
          return Map.empty
    _ -> do
      return Map.empty


parseFormURLEncoded :: String -> Map String String
parseFormURLEncoded input =
    let split [] = []
        split string = case elemIndex '&' string of
                         Nothing -> [string]
                         Just index ->
                             let (first, rest) = splitAt index string
                             in first : (split $ drop 1 rest)
        splitNameValuePair string = case elemIndex '=' string of
                                      Nothing -> (string, "")
                                      Just index -> let (first, rest)
                                                          = splitAt index string
                                                    in (first, drop 1 rest)
        evaluateEscapes' "" = []
        evaluateEscapes' ('%':a:b:rest) | isHexDigit a && isHexDigit b
                                           = fromIntegral ((digitToInt a * 16)
                                                           + (digitToInt b))
                                             : evaluateEscapes' rest
        evaluateEscapes' ('+':rest) = (fromIntegral $ ord ' ') : evaluateEscapes' rest
        evaluateEscapes' (c:rest) = (fromIntegral $ ord c) : evaluateEscapes' rest
        evaluateEscapes string = UTF8.toString $ BS.pack $ evaluateEscapes' string
        nameValuePairs = map (\(name, value)
                                  -> (evaluateEscapes name, evaluateEscapes value))
                             $ map splitNameValuePair $ split input
    in Map.fromList nameValuePairs


setMimeType :: FruitTart ()
setMimeType = do
  maybeAccept <- getRequestHeader HttpAccept
  let accept = case maybeAccept of
                 Nothing -> "text/html"
                 Just accept -> accept
  let split "" = []
      split string = case elemIndex ',' string of
                       Nothing -> [string]
                       Just index -> let (first, rest) = splitAt index string
                                     in first : (split $ drop 1 rest)
      trim string = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace string
      splitQValue string = case elemIndex ';' string of
                             Nothing -> (string, 1.0)
                             Just index -> let (first, rest) = splitAt index string
                                           in (first, parseQValue $ drop 1 rest)
      parseQValue ('q':'=':quantity) = let parses = readFloat quantity
                                         in case parses of
                                              [(result, "")] -> result
                                              _ -> 1.0
      parseQValue _ = 1.0
      qValueMap = Map.fromList $ map (splitQValue . trim) $ split accept
      defaultQValue = case Map.lookup "*/*" qValueMap of
                        Nothing -> 0.0
                        Just value -> value
      htmlQValue = case Map.lookup "text/html" qValueMap of
                     Nothing -> defaultQValue
                     Just value -> value
      xhtmlQValue = case Map.lookup "application/xhtml+xml" qValueMap of
                      Nothing -> 0.0
                      Just value -> value
  if xhtmlQValue >= htmlQValue
    then setResponseHeader HttpContentType "application/xhtml+xml; charset=UTF-8"
    else setResponseHeader HttpContentType "text/html; charset=UTF-8"


parseURL :: String -> (String, String, [String], Map String String)
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
    in (controller, action, urlParameters, Map.fromList namedParameters)


canonicalURL :: String -> String -> [String] -> Map String String -> String
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
                                           $ sortBy (comparing fst)
                                           $ Map.toList namedParameters
    in concat [basePart, urlParametersPart, namedParametersPart]


defaultController :: String
defaultController = "issues"


defaultAction :: String
defaultAction = "index"

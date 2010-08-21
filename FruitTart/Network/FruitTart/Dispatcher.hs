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
                           fCloseOutput
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
                                     ++ "has more details.</p></body></html>"
                           fCloseOutput))


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
          fCloseOutput
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
               fCloseOutput
               return ()
             Just (actionID, functionName) -> do
               (mandatoryParameterTypes,
                optionalParameterTypes,
                namedParameterTypes) <- lookupParameterTypes actionID
               let maybeParameters = decodeParameters controllerModuleName
                                                      unnamedParameters
                                                      namedParameters
                                                      mandatoryParameterTypes
                                                      optionalParameterTypes
                                                      namedParameterTypes
               case maybeParameters of
                 Nothing -> do
                    applyFunctionGivenName ControllerContext
                                           Map.empty
                                           "Base"
                                           "errorActionParameters"
                                           [CustardString controllerName,
                                            CustardString actionName]
                    fCloseOutput
                    return ()
                 Just parameters -> do
                   applyFunctionGivenName ControllerContext
                                          formInput
                                          controllerModuleName
                                          functionName
                                          parameters
                   fCloseOutput
                   return ()
  endSession


lookupController :: String -> FruitTart (Maybe String)
lookupController mappedName = do
  rows <- query "SELECT module FROM controllers WHERE mapped_name = ?"
                [SQLText mappedName]
  case rows of
    [] -> return Nothing
    [[SQLText moduleName]] -> return $ Just moduleName


lookupAction :: String -> String -> String -> FruitTart (Maybe (Int64, String))
lookupAction moduleName actionName method = do
  rows <- query ("SELECT id, function FROM controller_actions "
                 ++ "WHERE controller = ? AND action = ? AND method = ?")
                [SQLText moduleName, SQLText actionName, SQLText method]
  case rows of
    [] -> return Nothing
    [[SQLInteger actionID, SQLText functionName]]
      -> return $ Just (actionID, functionName)


lookupParameterTypes :: Int64
                     -> FruitTart ([ParameterType],
                                   [ParameterType],
                                   Map String ParameterType)
lookupParameterTypes actionID = do
  mandatoryParameterRows
    <- query ("SELECT type FROM action_mandatory_parameters "
              ++ "WHERE action = ? ORDER BY item")
             [SQLInteger actionID]
  optionalParameterRows
    <- query ("SELECT type FROM action_optional_parameters "
              ++ "WHERE action = ? ORDER BY item")
             [SQLInteger actionID]
  namedParameterRows
    <- query ("SELECT name, type FROM action_named_parameters "
              ++ "WHERE action = ?")
             [SQLInteger actionID]
  let decodeType "integer" = IntegerParameter
      decodeType "string" = StringParameter
      decodeType _ = StringParameter
      mandatoryParameterTypes = map (\[SQLText theType] -> decodeType theType)
                                    mandatoryParameterRows
      optionalParameterTypes = map (\[SQLText theType] -> decodeType theType)
                                   optionalParameterRows
      namedParameterTypeMap = Map.fromList
                              $ map (\[SQLText name, SQLText theType] ->
                                      (name, decodeType theType))
                                    namedParameterRows
  return (mandatoryParameterTypes,
          optionalParameterTypes,
          namedParameterTypeMap)


decodeParameters :: String
                 -> [String]
                 -> Map String String
                 -> [ParameterType]
                 -> [ParameterType]
                 -> Map String ParameterType
                 -> Maybe [CustardValue]
decodeParameters moduleName
                 unnamedParameterInputs
                 namedParameterInputs
                 mandatoryParameterTypes
                 optionalParameterTypes
                 namedParameterTypes =
  let nFormalMandatoryParameters = length mandatoryParameterTypes
      nFormalOptionalParameters = length optionalParameterTypes
      mandatoryParameterInputs
        = take nFormalMandatoryParameters unnamedParameterInputs
      optionalParameterInputs
        = drop nFormalMandatoryParameters unnamedParameterInputs
      nActualMandatoryParameters = length mandatoryParameterInputs
      nActualOptionalParameters = length optionalParameterInputs
      unknownNamedParameters
        = Map.difference namedParameterInputs namedParameterTypes
  in if (nActualMandatoryParameters /= nFormalMandatoryParameters)
        || (nActualOptionalParameters > nActualOptionalParameters)
        || (not $ Map.null unknownNamedParameters)
    then Nothing
    else let mandatoryZipped
               = zip mandatoryParameterInputs mandatoryParameterTypes
             optionalZipped
               = zip optionalParameterInputs optionalParameterTypes
             namedZipped
               = Map.mapWithKey (\key value ->
                                   (value,
                                    fromJust
                                    $ Map.lookup key namedParameterTypes))
                                namedParameterInputs
             mandatoryParametersValid
               = and $ map validateParameter mandatoryZipped
             optionalParametersValid
               = and $ map validateParameter optionalZipped
             namedParametersValid
               = and $ Map.elems $ Map.map validateParameter namedZipped
         in if not (mandatoryParametersValid
                    && optionalParametersValid
                    && namedParametersValid)
           then Nothing
           else let mandatoryParameters
                      = map readParameter mandatoryZipped
                    nOptionalNothings
                      = nFormalOptionalParameters - nActualOptionalParameters
                    optionalNothings
                      = take nOptionalNothings $ repeat $ CustardMaybe Nothing
                    optionalParameters
                      = map (CustardMaybe . Just . readParameter) optionalZipped
                        ++ optionalNothings
                    namedParameters
                      = Map.mapKeys (\properName -> (moduleName, properName))
                                    $ Map.map readParameter namedZipped
                    hasNamedParameters
                      = not $ Map.null namedParameterTypes
                in Just $ mandatoryParameters
                          ++ optionalParameters
                          ++ (if hasNamedParameters
                                then [CustardMap namedParameters]
                                else [])


validateParameter :: (String, ParameterType) -> Bool
validateParameter (id, IntegerParameter) = length id > 0 && all isDigit id
validateParameter (_, StringParameter) = True


readParameter :: (String, ParameterType) -> CustardValue
readParameter (id, IntegerParameter) = CustardInteger $ read id
readParameter (string, StringParameter) = CustardString string


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

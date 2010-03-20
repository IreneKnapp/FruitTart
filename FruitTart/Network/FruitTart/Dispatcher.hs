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
import Network.FastCGI
import Numeric
import Prelude hiding (catch)
import Random
import System.Time

import qualified Database.SQLite3 as SQL
import Network.FruitTart.Base
import Network.FruitTart.Util


processRequest :: ControllerTable -> FruitTart ()
processRequest dispatchTable  = do
  fCatch (processRequest' dispatchTable)
         (\e -> do
                  fLog $ "FruitTart: " ++ show (e :: SomeException)
                  error500)


processRequest' :: ControllerTable -> FruitTart ()
processRequest' dispatchTable = do
  initializeSession
  processFormInput
  callModuleInitRequestMethods
  setMimeType
  maybeURL <- getQueryString
  let url = fromJust maybeURL
  (controllerName, actionName, urlParameters, namedParameters)
        <- return $ parseURL url
  canonical <- return
        $ canonicalURL controllerName actionName urlParameters namedParameters
  maybeMethod <- getRequestMethod
  let method = fromJust maybeMethod
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
    -> FruitTart ()
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


processFormInput :: FruitTart ()
processFormInput = do
  state <- get
  maybeRequestMethod <- getRequestMethod
  case maybeRequestMethod of
    Just "POST" -> do
      maybeContentType <- getContentType
      case maybeContentType of
        Just "application/x-www-form-urlencoded" -> do
          inputData <- fGetContents
          let formVariableMap = parseFormURLEncoded $ UTF8.toString inputData
          liftIO $ swapMVar (formVariableMapMVar state) formVariableMap
        _ -> do
          liftIO $ swapMVar (formVariableMapMVar state) Map.empty
    _ -> do
      liftIO $ swapMVar (formVariableMapMVar state) Map.empty
  return ()


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


callModuleInitRequestMethods :: FruitTart ()
callModuleInitRequestMethods = do
  FruitTartState { interfacesMapMVar = interfacesMapMVar } <- get
  interfacesMap <- liftIO $ readMVar interfacesMapMVar
  mapM (\interface -> interfaceInitRequest interface)
       $ Map.elems interfacesMap
  return ()


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
    then setResponseHeader HttpContentType "application/xhtml+xml; charset=UTF8"
    else setResponseHeader HttpContentType "text/html; charset=UTF8"

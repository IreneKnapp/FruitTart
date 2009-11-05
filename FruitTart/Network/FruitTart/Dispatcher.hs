{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Network.FruitTart.Dispatcher (processRequest) where

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

import qualified Database.SQLite3 as SQL
import Network.FruitTart.Util


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

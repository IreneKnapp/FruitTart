module Network.FruitTart.Common (
                                 getCaptchaCacheMVar,
                                 permanentRedirect,
                                 seeOtherRedirect,
                                 error404,
                                 error500,
                                 errorControllerUndefined,
                                 errorActionUndefined,
                                 errorActionParameters,
                                 errorInvalidID,
                                 getSessionID
                                )
    where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.FastCGI

import Network.FruitTart.Types
import Network.FruitTart.Util


getCaptchaCacheMVar :: FruitTart (MVar (Map Int64 (String, ByteString)))
getCaptchaCacheMVar = do
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  return captchaCacheMVar


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


errorActionUndefined :: String -> String -> String -> FruitTart ()
errorActionUndefined controllerName actionName method =
    error404 $ "No action named named " ++ actionName
             ++ " with the HTTP method " ++ method
             ++ "is defined for the controller " ++ controllerName ++ "."


errorActionParameters :: String -> String -> FruitTart ()
errorActionParameters controllerName actionName =
    error404 $ "Invalid parameters to the action " ++ actionName
             ++ " of the controller " ++ controllerName ++ "."


errorInvalidID :: String -> FruitTart ()
errorInvalidID kind =
    error404 $ "No " ++ kind ++ " by that ID exists." 


getSessionID :: FruitTart Int64
getSessionID = do
  FruitTartState { sessionID = sessionID } <- get
  return $ fromJust sessionID

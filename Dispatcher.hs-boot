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

import Control.Exception
import Data.Char
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Typeable
import Network.FastCGI hiding (output)
import Prelude hiding (catch)

import Types


data Abort = Abort

processRequest :: Buglist CGIResult
output :: String -> Buglist CGIResult
abort :: Buglist CGIResult
permanentRedirect :: String -> Buglist CGIResult
seeOtherRedirect :: String -> Buglist CGIResult
error404 :: String -> Buglist CGIResult
error500 :: Buglist CGIResult
errorControllerUndefined :: String -> Buglist CGIResult
errorActionUndefined :: String -> String -> Buglist CGIResult
errorActionParameters :: String -> String -> Buglist CGIResult
errorActionMethod :: String -> String -> String -> Buglist CGIResult
errorInvalidID :: String -> Buglist CGIResult
parseURL :: String -> (String, String, [String], [(String, String)])
canonicalURL :: String -> String -> [String] -> [(String, String)] -> String
getSessionID :: Buglist Int64

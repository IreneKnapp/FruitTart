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

import Control.Exception
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Typeable
import Network.FastCGI hiding (output)
import Prelude hiding (catch)


data Abort = Abort

main :: IO ()
processRequest :: CGI CGIResult
output :: String -> CGI CGIResult
abort :: CGI CGIResult
permanentRedirect :: String -> CGI CGIResult
error404 :: String -> CGI CGIResult
errorControllerUndefined :: String -> CGI CGIResult
errorActionUndefined :: String -> String -> CGI CGIResult
errorActionParameters :: String -> String -> CGI CGIResult
errorTemplateUndefined :: String -> CGI CGIResult
parseURL :: String -> (String, String, [String], [(String, String)])
canonicalURL :: String -> String -> [String] -> [(String, String)] -> String

module Network.FruitTart.Custard.Functions.HTTP (
                                                 cfLog,
                                                 cfGetRequestVariable,
                                                 cfGetAllRequestVariables,
                                                 cfGetRequestHeader,
                                                 cfCookieName,
                                                 cfCookieValue,
                                                 cfCookieVersion,
                                                 cfCookiePath,
                                                 cfCookieDomain,
                                                 cfCookieMaxAge,
                                                 cfCookieSecure,
                                                 cfCookieComment,
                                                 cfGetCookie,
                                                 cfGetAllCookies,
                                                 cfGetCookieValue,
                                                 cfGetDocumentRoot,
                                                 cfGetGatewayInterface,
                                                 cfGetPathInfo,
                                                 cfGetPathTranslated,
                                                 cfGetQueryString,
                                                 cfGetRedirectStatus,
                                                 cfGetRedirectURI,
                                                 cfGetRemoteAddress,
                                                 cfGetRemotePort,
                                                 cfGetRemoteHost,
                                                 cfGetRemoteIdent,
                                                 cfGetRemoteUser,
                                                 cfGetRequestMethod,
                                                 cfGetRequestURI,
                                                 cfGetScriptFilename,
                                                 cfGetScriptName,
                                                 cfGetServerAddress,
                                                 cfGetServerName,
                                                 cfGetServerPort,
                                                 cfGetServerProtocol,
                                                 cfGetServerSoftware,
                                                 cfGetAuthenticationType,
                                                 cfGetContentLength,
                                                 cfGetContentType,
                                                 cfSetResponseStatus,
                                                 cfGetResponseStatus,
                                                 cfSetResponseHeader,
                                                 cfUnsetResponseHeader,
                                                 cfGetResponseHeader,
                                                 cfSetCookie,
                                                 cfUnsetCookie,
                                                 cfMakeSimpleCookie,
                                                 cfMakeCookie,
                                                 cfPermanentRedirect,
                                                 cfSeeOtherRedirect,
                                                 cfSendResponseHeaders,
                                                 cfResponseHeadersSent,
                                                 cfPut,
                                                 cfPutString,
                                                 cfCloseOutput
                                                )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import qualified Network.FastCGI as FCGI

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfLog :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfLog context parameters = do
  requireControllerContext context "log"
  requireNParameters parameters 1 "log"
  string <- valueToString $ head parameters
  fLog string
  return CustardNull


cfGetRequestVariable :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfGetRequestVariable context parameters = do
  requireControllerContext context "getRequestVariable"
  requireNParameters parameters 1 "getRequestVariable"
  name <- valueToString $ parameters !! 0
  maybeValue <- getRequestVariable name
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetAllRequestVariables :: CustardContext
                         -> [CustardValue]
                         -> FruitTart CustardValue
cfGetAllRequestVariables context parameters = do
  requireControllerContext context "getAllRequestVariables"
  requireNParameters parameters 0 "getAllRequestVariables"
  variables <- getAllRequestVariables
  return $ CustardList
         $ map (\(name, value) ->
                  CustardTuple [CustardString name, CustardString value])
               variables


cfGetRequestHeader :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfGetRequestHeader context parameters = do
  requireControllerContext context "getRequestHeader"
  requireNParameters parameters 1 "getRequestHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  maybeValue <- getRequestHeader header
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfCookieName :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfCookieName context parameters = do
  requireControllerContext context "cookieName"
  requireNParameters parameters 1 "cookieName"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardString $ cookieName cookie


cfCookieValue :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfCookieValue context parameters = do
  requireControllerContext context "cookieValue"
  requireNParameters parameters 1 "cookieValue"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardString $ cookieValue cookie


cfCookieVersion :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfCookieVersion context parameters = do
  requireControllerContext context "cookieVersion"
  requireNParameters parameters 1 "cookieVersion"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ cookieVersion cookie


cfCookiePath :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfCookiePath context parameters = do
  requireControllerContext context "cookiePath"
  requireNParameters parameters 1 "cookiePath"
  cookie <- valueToHTTPCookie $ parameters !! 0
  case cookiePath cookie of
    Nothing -> return $ CustardMaybe Nothing
    Just path -> return $ CustardMaybe $ Just $ CustardString path


cfCookieDomain :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfCookieDomain context parameters = do
  requireControllerContext context "cookieDomain"
  requireNParameters parameters 1 "cookieDomain"
  cookie <- valueToHTTPCookie $ parameters !! 0
  case cookieDomain cookie of
    Nothing -> return $ CustardMaybe Nothing
    Just domain -> return $ CustardMaybe $ Just $ CustardString domain


cfCookieMaxAge :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfCookieMaxAge context parameters = do
  requireControllerContext context "cookieMaxAge"
  requireNParameters parameters 1 "cookieMaxAge"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardMaybe $ case cookieMaxAge cookie of
    Nothing -> Nothing
    Just maxAge -> Just $ CustardInteger $ fromIntegral maxAge


cfCookieSecure :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfCookieSecure context parameters = do
  requireControllerContext context "cookieSecure"
  requireNParameters parameters 1 "cookieSecure"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardBool $ cookieSecure cookie


cfCookieComment :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfCookieComment context parameters = do
  requireControllerContext context "cookieComment"
  requireNParameters parameters 1 "cookieComment"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return $ CustardMaybe $ case cookieComment cookie of
    Nothing -> Nothing
    Just comment -> Just $ CustardString comment


cfGetCookie :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfGetCookie context parameters = do
  requireControllerContext context "getCookie"
  requireNParameters parameters 1 "getCookie"
  name <- valueToString $ parameters !! 0
  maybeCookie <- getCookie name
  return $ CustardMaybe $ case maybeCookie of
    Nothing -> Nothing
    Just cookie -> Just $ CustardHTTPCookie cookie


cfGetAllCookies :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetAllCookies context parameters = do
  requireControllerContext context "getAllCookies"
  requireNParameters parameters 0 "getAllCookies"
  cookies <- getAllCookies
  return $ CustardList $ map CustardHTTPCookie cookies


cfGetCookieValue :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfGetCookieValue context parameters = do
  requireControllerContext context "getCookieValue"
  requireNParameters parameters 1 "getCookieValue"
  name <- valueToString $ parameters !! 0
  maybeValue <- getCookieValue name
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetDocumentRoot :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfGetDocumentRoot context parameters = do
  requireControllerContext context "getDocumentRoot"
  requireNParameters parameters 0 "getDocumentRoot"
  maybeValue <- getDocumentRoot
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetGatewayInterface :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfGetGatewayInterface context parameters = do
  requireControllerContext context "getGatewayInterface"
  requireNParameters parameters 0 "getGatewayInterface"
  maybeValue <- getGatewayInterface
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetPathInfo :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfGetPathInfo context parameters = do
  requireControllerContext context "getPathInfo"
  requireNParameters parameters 0 "getPathInfo"
  maybeValue <- getPathInfo
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetPathTranslated :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetPathTranslated context parameters = do
  requireControllerContext context "getPathTranslated"
  requireNParameters parameters 0 "getPathTranslated"
  maybeValue <- getPathTranslated
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetQueryString :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfGetQueryString context parameters = do
  requireControllerContext context "getQueryString"
  requireNParameters parameters 0 "getQueryString"
  maybeValue <- getQueryString
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRedirectStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetRedirectStatus context parameters = do
  requireControllerContext context "getRedirectStatus"
  requireNParameters parameters 0 "getRedirectStatus"
  maybeValue <- getRedirectStatus
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardInteger $ fromIntegral value


cfGetRedirectURI :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfGetRedirectURI context parameters = do
  requireControllerContext context "getRedirectURI"
  requireNParameters parameters 0 "getRedirectURI"
  maybeValue <- getRedirectURI
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRemoteAddress :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfGetRemoteAddress context parameters = do
  requireControllerContext context "getRemoteAddress"
  requireNParameters parameters 0 "getRemoteAddress"
  maybeValue <- getRemoteAddress
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardHostAddress value


cfGetRemotePort :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetRemotePort context parameters = do
  requireControllerContext context "getRemotePort"
  requireNParameters parameters 0 "getRemotePort"
  maybeValue <- getRemotePort
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardInteger $ fromIntegral value


cfGetRemoteHost :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetRemoteHost context parameters = do
  requireControllerContext context "getRemoteHost"
  requireNParameters parameters 0 "getRemoteHost"
  maybeValue <- getRemoteHost
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRemoteIdent :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfGetRemoteIdent context parameters = do
  requireControllerContext context "getRemoteIdent"
  requireNParameters parameters 0 "getRemoteIdent"
  maybeValue <- getRemoteIdent
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRemoteUser :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetRemoteUser context parameters = do
  requireControllerContext context "getRemoteUser"
  requireNParameters parameters 0 "getRemoteUser"
  maybeValue <- getRemoteUser
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRequestMethod :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfGetRequestMethod context parameters = do
  requireControllerContext context "getRequestMethod"
  requireNParameters parameters 0 "getRequestMethod"
  maybeValue <- getRequestMethod
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetRequestURI :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetRequestURI context parameters = do
  requireControllerContext context "getRequestURI"
  requireNParameters parameters 0 "getRequestURI"
  maybeValue <- getRequestURI
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetScriptFilename :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetScriptFilename context parameters = do
  requireControllerContext context "getScriptFilename"
  requireNParameters parameters 0 "getScriptFilename"
  maybeValue <- getScriptFilename
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetScriptName :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetScriptName context parameters = do
  requireControllerContext context "getScriptName"
  requireNParameters parameters 0 "getScriptName"
  maybeValue <- getScriptName
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetServerAddress :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfGetServerAddress context parameters = do
  requireControllerContext context "getServerAddress"
  requireNParameters parameters 0 "getServerAddress"
  maybeValue <- getServerAddress
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardHostAddress value


cfGetServerName :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetServerName context parameters = do
  requireControllerContext context "getServerName"
  requireNParameters parameters 0 "getServerName"
  maybeValue <- getServerName
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetServerPort :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfGetServerPort context parameters = do
  requireControllerContext context "getServerPort"
  requireNParameters parameters 0 "getServerPort"
  maybeValue <- getServerPort
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardInteger $ fromIntegral value


cfGetServerProtocol :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetServerProtocol context parameters = do
  requireControllerContext context "getServerProtocol"
  requireNParameters parameters 0 "getServerProtocol"
  maybeValue <- getServerProtocol
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetServerSoftware :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetServerSoftware context parameters = do
  requireControllerContext context "getServerSoftware"
  requireNParameters parameters 0 "getServerSoftware"
  maybeValue <- getServerSoftware
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetAuthenticationType :: CustardContext
                        -> [CustardValue]
                        -> FruitTart CustardValue
cfGetAuthenticationType context parameters = do
  requireControllerContext context "getAuthenticationType"
  requireNParameters parameters 0 "getAuthenticationType"
  maybeValue <- getAuthenticationType
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfGetContentLength :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfGetContentLength context parameters = do
  requireControllerContext context "getContentLength"
  requireNParameters parameters 0 "getContentLength"
  maybeValue <- getContentLength
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardInteger $ fromIntegral value


cfGetContentType :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfGetContentType context parameters = do
  requireControllerContext context "getContentType"
  requireNParameters parameters 0 "getContentType"
  maybeValue <- getContentType
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfSetResponseStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfSetResponseStatus context parameters = do
  requireControllerContext context "setResponseStatus"
  requireNParameters parameters 1 "setResponseStatus"
  value <- valueToInteger $ head parameters
  setResponseStatus $ fromIntegral value
  return CustardNull


cfGetResponseStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetResponseStatus context parameters = do
  requireControllerContext context "getResponseStatus"
  requireNParameters parameters 0 "getResponseStatus"
  value <- getResponseStatus
  return $ CustardInteger $ fromIntegral value


cfSetResponseHeader :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfSetResponseHeader context parameters = do
  requireControllerContext context "setResponseHeader"
  requireNParameters parameters 2 "setResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  value <- valueToString $ parameters !! 1
  setResponseHeader header value
  return CustardNull


cfUnsetResponseHeader :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfUnsetResponseHeader context parameters = do
  requireControllerContext context "unsetResponseHeader"
  requireNParameters parameters 1 "unsetResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  unsetResponseHeader header
  return CustardNull


cfGetResponseHeader :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfGetResponseHeader context parameters = do
  requireControllerContext context "getResponseHeader"
  requireNParameters parameters 1 "getResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  maybeValue <- getResponseHeader header
  return $ CustardMaybe $ case maybeValue of
    Nothing -> Nothing
    Just value -> Just $ CustardString value


cfSetCookie :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfSetCookie context parameters = do
  requireControllerContext context "setCookie"
  requireNParameters parameters 1 "setCookie"
  cookie <- valueToHTTPCookie $ parameters !! 0
  setCookie cookie
  return CustardNull


cfUnsetCookie :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfUnsetCookie context parameters = do
  requireControllerContext context "unsetCookie"
  requireNParameters parameters 1 "unsetCookie"
  name <- valueToString $ parameters !! 0
  unsetCookie name
  return CustardNull


cfMakeSimpleCookie :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMakeSimpleCookie context parameters = do
  requireControllerContext context "makeSimpleCookie"
  requireNParameters parameters 2 "makeSimpleCookie"
  name <- valueToString $ parameters !! 0
  value <- valueToString $ parameters !! 1
  let cookie = mkSimpleCookie name value
  return $ CustardHTTPCookie cookie


cfMakeCookie :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMakeCookie context parameters = do
  requireControllerContext context "makeCookie"
  requireNParameters parameters 6 "makeCookie"
  name <- valueToString $ parameters !! 0
  value <- valueToString $ parameters !! 1
  maybePath <- valueToMaybeString $ parameters !! 2
  maybeDomain <- valueToMaybeString $ parameters !! 3
  maybeMaxAge <- valueToMaybeInteger $ parameters !! 4
  let maybeMaxAge' = case maybeMaxAge of
                       Nothing -> Nothing
                       Just maxAge -> Just $ fromIntegral maxAge
  secure <- valueToBoolean $ parameters !! 5
  let cookie = mkCookie name value maybePath maybeDomain maybeMaxAge' secure
  return $ CustardHTTPCookie cookie


cfPermanentRedirect :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfPermanentRedirect context parameters = do
  requireControllerContext context "permanentRedirect"
  requireNParameters parameters 1 "permanentRedirect"
  url <- valueToString $ parameters !! 0
  permanentRedirect url
  return CustardNull


cfSeeOtherRedirect :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfSeeOtherRedirect context parameters = do
  requireControllerContext context "seeOtherRedirect"
  requireNParameters parameters 1 "seeOtherRedirect"
  url <- valueToString $ parameters !! 0
  seeOtherRedirect url
  return CustardNull


cfSendResponseHeaders :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfSendResponseHeaders context parameters = do
  requireControllerContext context "sendResponseHeaders"
  requireNParameters parameters 0 "sendResponseHeaders"
  sendResponseHeaders
  return CustardNull


cfResponseHeadersSent :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfResponseHeadersSent context parameters = do
  requireControllerContext context "responseHeadersSent"
  requireNParameters parameters 0 "responseHeadersSent"
  result <- responseHeadersSent
  return $ CustardBool result


cfPut :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfPut context parameters = do
  requireControllerContext context "put"
  requireNParameters parameters 1 "put"
  byteString <- valueToByteString $ parameters !! 0
  fPut byteString
  return CustardNull


cfPutString :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfPutString context parameters = do
  requireControllerContext context "putString"
  requireNParameters parameters 1 "putString"
  string <- valueToString $ parameters !! 0
  fPutStr string
  return CustardNull


cfCloseOutput :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfCloseOutput context parameters = do
  requireControllerContext context "closeOutput"
  requireNParameters parameters 0 "closeOutput"
  fCloseOutput
  return CustardNull

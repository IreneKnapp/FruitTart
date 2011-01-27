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
import qualified Data.ByteString.UTF8 as UTF8
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


cfLog :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfLog context parameters = do
  requireControllerContext context "log"
  requireNParameters parameters 1 "log"
  bytestring <- valueToUTF8String $ head parameters
  FCGI.fLog $ UTF8.toString bytestring
  return (context, CustardNull)


cfGetRequestVariable :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfGetRequestVariable context parameters = do
  requireControllerContext context "getRequestVariable"
  requireNParameters parameters 1 "getRequestVariable"
  name <- valueToUTF8String $ parameters !! 0
  maybeValue <- FCGI.getRequestVariable $ UTF8.toString name
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetAllRequestVariables :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfGetAllRequestVariables context parameters = do
  requireControllerContext context "getAllRequestVariables"
  requireNParameters parameters 0 "getAllRequestVariables"
  variables <- FCGI.getAllRequestVariables
  return (context,
          CustardList
           $ map (\(name, value) ->
                    CustardTuple [CustardString $ UTF8.fromString name,
                                  CustardString $ UTF8.fromString value])
                 variables)


cfGetRequestHeader :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfGetRequestHeader context parameters = do
  requireControllerContext context "getRequestHeader"
  requireNParameters parameters 1 "getRequestHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  maybeValue <- FCGI.getRequestHeader header
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfCookieName :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfCookieName context parameters = do
  requireControllerContext context "cookieName"
  requireNParameters parameters 1 "cookieName"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context, CustardString $ UTF8.fromString $ FCGI.cookieName cookie)


cfCookieValue :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfCookieValue context parameters = do
  requireControllerContext context "cookieValue"
  requireNParameters parameters 1 "cookieValue"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context, CustardString $ UTF8.fromString $ FCGI.cookieValue cookie)


cfCookieVersion :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfCookieVersion context parameters = do
  requireControllerContext context "cookieVersion"
  requireNParameters parameters 1 "cookieVersion"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ FCGI.cookieVersion cookie)


cfCookiePath :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfCookiePath context parameters = do
  requireControllerContext context "cookiePath"
  requireNParameters parameters 1 "cookiePath"
  cookie <- valueToHTTPCookie $ parameters !! 0
  case FCGI.cookiePath cookie of
    Nothing -> return (context, CustardMaybe Nothing)
    Just path -> return (context, CustardMaybe $ Just $ CustardString
                                                      $ UTF8.fromString path)


cfCookieDomain :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfCookieDomain context parameters = do
  requireControllerContext context "cookieDomain"
  requireNParameters parameters 1 "cookieDomain"
  cookie <- valueToHTTPCookie $ parameters !! 0
  case FCGI.cookieDomain cookie of
    Nothing -> return (context, CustardMaybe Nothing)
    Just domain -> return (context,
                           CustardMaybe $ Just $ CustardString
                                               $ UTF8.fromString domain)


cfCookieMaxAge :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfCookieMaxAge context parameters = do
  requireControllerContext context "cookieMaxAge"
  requireNParameters parameters 1 "cookieMaxAge"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context,
          CustardMaybe $ case FCGI.cookieMaxAge cookie of
                           Nothing -> Nothing
                           Just maxAge -> Just $ CustardInteger
                                               $ fromIntegral maxAge)


cfCookieSecure :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfCookieSecure context parameters = do
  requireControllerContext context "cookieSecure"
  requireNParameters parameters 1 "cookieSecure"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context, CustardBool $ FCGI.cookieSecure cookie)


cfCookieComment :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfCookieComment context parameters = do
  requireControllerContext context "cookieComment"
  requireNParameters parameters 1 "cookieComment"
  cookie <- valueToHTTPCookie $ parameters !! 0
  return (context,
          CustardMaybe $ case FCGI.cookieComment cookie of
                           Nothing -> Nothing
                           Just comment -> Just $ CustardString
                                                $ UTF8.fromString comment)


cfGetCookie :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfGetCookie context parameters = do
  requireControllerContext context "getCookie"
  requireNParameters parameters 1 "getCookie"
  name <- valueToUTF8String $ parameters !! 0
  maybeCookie <- FCGI.getCookie $ UTF8.toString name
  return (context,
          CustardMaybe $ case maybeCookie of
                           Nothing -> Nothing
                           Just cookie -> Just $ CustardHTTPCookie cookie)


cfGetAllCookies :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetAllCookies context parameters = do
  requireControllerContext context "getAllCookies"
  requireNParameters parameters 0 "getAllCookies"
  cookies <- FCGI.getAllCookies
  return (context, CustardList $ map CustardHTTPCookie cookies)


cfGetCookieValue :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetCookieValue context parameters = do
  requireControllerContext context "getCookieValue"
  requireNParameters parameters 1 "getCookieValue"
  name <- valueToUTF8String $ parameters !! 0
  maybeValue <- FCGI.getCookieValue $ UTF8.toString name
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetDocumentRoot :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfGetDocumentRoot context parameters = do
  requireControllerContext context "getDocumentRoot"
  requireNParameters parameters 0 "getDocumentRoot"
  maybeValue <- FCGI.getDocumentRoot
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetGatewayInterface :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfGetGatewayInterface context parameters = do
  requireControllerContext context "getGatewayInterface"
  requireNParameters parameters 0 "getGatewayInterface"
  maybeValue <- FCGI.getGatewayInterface
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetPathInfo :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfGetPathInfo context parameters = do
  requireControllerContext context "getPathInfo"
  requireNParameters parameters 0 "getPathInfo"
  maybeValue <- FCGI.getPathInfo
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetPathTranslated :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetPathTranslated context parameters = do
  requireControllerContext context "getPathTranslated"
  requireNParameters parameters 0 "getPathTranslated"
  maybeValue <- FCGI.getPathTranslated
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetQueryString :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetQueryString context parameters = do
  requireControllerContext context "getQueryString"
  requireNParameters parameters 0 "getQueryString"
  maybeValue <- FCGI.getQueryString
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRedirectStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetRedirectStatus context parameters = do
  requireControllerContext context "getRedirectStatus"
  requireNParameters parameters 0 "getRedirectStatus"
  maybeValue <- FCGI.getRedirectStatus
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardInteger
                                              $ fromIntegral value)


cfGetRedirectURI :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetRedirectURI context parameters = do
  requireControllerContext context "getRedirectURI"
  requireNParameters parameters 0 "getRedirectURI"
  maybeValue <- FCGI.getRedirectURI
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRemoteAddress :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfGetRemoteAddress context parameters = do
  requireControllerContext context "getRemoteAddress"
  requireNParameters parameters 0 "getRemoteAddress"
  maybeValue <- FCGI.getRemoteAddress
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardHostAddress value)


cfGetRemotePort :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetRemotePort context parameters = do
  requireControllerContext context "getRemotePort"
  requireNParameters parameters 0 "getRemotePort"
  maybeValue <- FCGI.getRemotePort
  return (context, CustardMaybe $ case maybeValue of
                                    Nothing -> Nothing
                                    Just value -> Just $ CustardInteger
                                                       $ fromIntegral value)


cfGetRemoteHost :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetRemoteHost context parameters = do
  requireControllerContext context "getRemoteHost"
  requireNParameters parameters 0 "getRemoteHost"
  maybeValue <- FCGI.getRemoteHost
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRemoteIdent :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetRemoteIdent context parameters = do
  requireControllerContext context "getRemoteIdent"
  requireNParameters parameters 0 "getRemoteIdent"
  maybeValue <- FCGI.getRemoteIdent
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRemoteUser :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetRemoteUser context parameters = do
  requireControllerContext context "getRemoteUser"
  requireNParameters parameters 0 "getRemoteUser"
  maybeValue <- FCGI.getRemoteUser
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRequestMethod :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfGetRequestMethod context parameters = do
  requireControllerContext context "getRequestMethod"
  requireNParameters parameters 0 "getRequestMethod"
  maybeValue <- FCGI.getRequestMethod
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetRequestURI :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetRequestURI context parameters = do
  requireControllerContext context "getRequestURI"
  requireNParameters parameters 0 "getRequestURI"
  maybeValue <- FCGI.getRequestURI
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetScriptFilename :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetScriptFilename context parameters = do
  requireControllerContext context "getScriptFilename"
  requireNParameters parameters 0 "getScriptFilename"
  maybeValue <- FCGI.getScriptFilename
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetScriptName :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetScriptName context parameters = do
  requireControllerContext context "getScriptName"
  requireNParameters parameters 0 "getScriptName"
  maybeValue <- FCGI.getScriptName
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetServerAddress :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfGetServerAddress context parameters = do
  requireControllerContext context "getServerAddress"
  requireNParameters parameters 0 "getServerAddress"
  maybeValue <- FCGI.getServerAddress
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardHostAddress value)


cfGetServerName :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetServerName context parameters = do
  requireControllerContext context "getServerName"
  requireNParameters parameters 0 "getServerName"
  maybeValue <- FCGI.getServerName
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetServerPort :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetServerPort context parameters = do
  requireControllerContext context "getServerPort"
  requireNParameters parameters 0 "getServerPort"
  maybeValue <- FCGI.getServerPort
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardInteger
                                              $ fromIntegral value)


cfGetServerProtocol :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetServerProtocol context parameters = do
  requireControllerContext context "getServerProtocol"
  requireNParameters parameters 0 "getServerProtocol"
  maybeValue <- FCGI.getServerProtocol
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetServerSoftware :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetServerSoftware context parameters = do
  requireControllerContext context "getServerSoftware"
  requireNParameters parameters 0 "getServerSoftware"
  maybeValue <- FCGI.getServerSoftware
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetAuthenticationType :: CustardContext
                        -> [CustardValue]
                        -> FruitTart (CustardContext, CustardValue)
cfGetAuthenticationType context parameters = do
  requireControllerContext context "getAuthenticationType"
  requireNParameters parameters 0 "getAuthenticationType"
  maybeValue <- FCGI.getAuthenticationType
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfGetContentLength :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfGetContentLength context parameters = do
  requireControllerContext context "getContentLength"
  requireNParameters parameters 0 "getContentLength"
  maybeValue <- FCGI.getContentLength
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardInteger
                                              $ fromIntegral value)


cfGetContentType :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetContentType context parameters = do
  requireControllerContext context "getContentType"
  requireNParameters parameters 0 "getContentType"
  maybeValue <- FCGI.getContentType
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfSetResponseStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfSetResponseStatus context parameters = do
  requireControllerContext context "setResponseStatus"
  requireNParameters parameters 1 "setResponseStatus"
  value <- valueToInteger $ head parameters
  FCGI.setResponseStatus $ fromIntegral value
  return (context, CustardNull)


cfGetResponseStatus :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetResponseStatus context parameters = do
  requireControllerContext context "getResponseStatus"
  requireNParameters parameters 0 "getResponseStatus"
  value <- FCGI.getResponseStatus
  return (context, CustardInteger $ fromIntegral value)


cfSetResponseHeader :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfSetResponseHeader context parameters = do
  requireControllerContext context "setResponseHeader"
  requireNParameters parameters 2 "setResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  value <- valueToUTF8String $ parameters !! 1
  FCGI.setResponseHeader header $ UTF8.toString value
  return (context, CustardNull)


cfUnsetResponseHeader :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfUnsetResponseHeader context parameters = do
  requireControllerContext context "unsetResponseHeader"
  requireNParameters parameters 1 "unsetResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  FCGI.unsetResponseHeader header
  return (context, CustardNull)


cfGetResponseHeader :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfGetResponseHeader context parameters = do
  requireControllerContext context "getResponseHeader"
  requireNParameters parameters 1 "getResponseHeader"
  header <- valueToHTTPHeader $ parameters !! 0
  maybeValue <- FCGI.getResponseHeader header
  return (context,
          CustardMaybe $ case maybeValue of
                           Nothing -> Nothing
                           Just value -> Just $ CustardString
                                              $ UTF8.fromString value)


cfSetCookie :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfSetCookie context parameters = do
  requireControllerContext context "setCookie"
  requireNParameters parameters 1 "setCookie"
  cookie <- valueToHTTPCookie $ parameters !! 0
  FCGI.setCookie cookie
  return (context, CustardNull)


cfUnsetCookie :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfUnsetCookie context parameters = do
  requireControllerContext context "unsetCookie"
  requireNParameters parameters 1 "unsetCookie"
  name <- valueToUTF8String $ parameters !! 0
  FCGI.unsetCookie $ UTF8.toString name
  return (context, CustardNull)


cfMakeSimpleCookie :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMakeSimpleCookie context parameters = do
  requireControllerContext context "makeSimpleCookie"
  requireNParameters parameters 2 "makeSimpleCookie"
  name <- valueToUTF8String $ parameters !! 0
  value <- valueToUTF8String $ parameters !! 1
  let cookie = FCGI.mkSimpleCookie (UTF8.toString name) (UTF8.toString value)
  return (context, CustardHTTPCookie cookie)


cfMakeCookie :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMakeCookie context parameters = do
  requireControllerContext context "makeCookie"
  requireNParameters parameters 6 "makeCookie"
  name <- valueToUTF8String $ parameters !! 0
  value <- valueToUTF8String $ parameters !! 1
  maybePath <- valueToMaybeUTF8String $ parameters !! 2
  maybeDomain <- valueToMaybeUTF8String $ parameters !! 3
  maybeMaxAge <- valueToMaybeInteger $ parameters !! 4
  let maybeMaxAge' = case maybeMaxAge of
                       Nothing -> Nothing
                       Just maxAge -> Just $ fromIntegral maxAge
  secure <- valueToBoolean $ parameters !! 5
  let cookie = FCGI.mkCookie (UTF8.toString name)
                             (UTF8.toString value)
                             (fmap UTF8.toString maybePath)
                             (fmap UTF8.toString maybeDomain)
                             maybeMaxAge' secure
  return (context, CustardHTTPCookie cookie)


cfPermanentRedirect :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfPermanentRedirect context parameters = do
  requireControllerContext context "permanentRedirect"
  requireNParameters parameters 1 "permanentRedirect"
  url <- valueToUTF8String $ parameters !! 0
  FCGI.permanentRedirect $ UTF8.toString url
  return (context, CustardNull)


cfSeeOtherRedirect :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfSeeOtherRedirect context parameters = do
  requireControllerContext context "seeOtherRedirect"
  requireNParameters parameters 1 "seeOtherRedirect"
  url <- valueToUTF8String $ parameters !! 0
  FCGI.seeOtherRedirect $ UTF8.toString url
  return (context, CustardNull)


cfSendResponseHeaders :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfSendResponseHeaders context parameters = do
  requireControllerContext context "sendResponseHeaders"
  requireNParameters parameters 0 "sendResponseHeaders"
  FCGI.sendResponseHeaders
  return (context, CustardNull)


cfResponseHeadersSent :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfResponseHeadersSent context parameters = do
  requireControllerContext context "responseHeadersSent"
  requireNParameters parameters 0 "responseHeadersSent"
  result <- FCGI.responseHeadersSent
  return (context, CustardBool result)


cfPut :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfPut context parameters = do
  requireControllerContext context "put"
  requireNParameters parameters 1 "put"
  byteString <- valueToByteString $ parameters !! 0
  FCGI.fPut byteString
  return (context, CustardNull)


cfPutString :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfPutString context parameters = do
  requireControllerContext context "putString"
  requireNParameters parameters 1 "putString"
  string <- valueToUTF8String $ parameters !! 0
  FCGI.fPut string
  return (context, CustardNull)


cfCloseOutput :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfCloseOutput context parameters = do
  requireControllerContext context "closeOutput"
  requireNParameters parameters 0 "closeOutput"
  FCGI.fCloseOutput
  return (context, CustardNull)

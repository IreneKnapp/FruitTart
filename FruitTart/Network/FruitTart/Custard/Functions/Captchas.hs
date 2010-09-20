module Network.FruitTart.Custard.Functions.Captchas (
                                                     cfGenerateCaptcha,
                                                     cfLookupCaptcha,
                                                     cfCheckCaptcha,
                                                     cfExpireOldCaptchas
                                                    )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import Graphics.Captcha

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfGenerateCaptcha :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfGenerateCaptcha context parameters = do
  requireControllerContext context "generateCaptcha"
  requireNParameters parameters 0 "generateCaptcha"
  expireOldCaptchas
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  timestamp <- getTimestamp
  (string, byteString) <- liftIO $ makeCaptcha
  captchaCache' <- return $ Map.insert timestamp (string, byteString) captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return (context, CustardInteger timestamp)


cfLookupCaptcha :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfLookupCaptcha context parameters = do
  requireControllerContext context "lookupCaptcha"
  requireNParameters parameters 1 "checkCaptcha"
  timestamp <- valueToInteger $ head parameters
  expireOldCaptchas
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ readMVar captchaCacheMVar
  captcha <- return $ Map.lookup timestamp captchaCache
  return (context,
          CustardMaybe $ case captcha of
                           Nothing -> Nothing
                           Just (_, bytestring) ->
                             Just $ CustardData bytestring)


cfCheckCaptcha :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfCheckCaptcha context parameters = do
  requireControllerContext context "checkCaptcha"
  requireNParameters parameters 2 "checkCaptcha"
  timestamp <- valueToInteger $ parameters !! 0
  responseString <- valueToUTF8String $ parameters !! 1
  expireOldCaptchas
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captcha <- return $ Map.lookup timestamp captchaCache
  captchaCache' <- return $ Map.delete timestamp captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return (context,
          CustardBool $ case captcha of
                          Nothing -> False
                          Just (challengeString, _)
                            -> (map toUpper $ UTF8.toString responseString)
                               == challengeString)


cfExpireOldCaptchas :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfExpireOldCaptchas context parameters = do
  requireControllerContext context "expireOldCaptchas"
  requireNParameters parameters 0 "expireOldCaptchas"
  expireOldCaptchas
  return (context, CustardNull)


expireOldCaptchas :: FruitTart ()
expireOldCaptchas = do
  currentTimestamp <- getTimestamp
  FruitTartState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captchaCache' <- return $ Map.filterWithKey (\captchaTimestamp _
                                               -> currentTimestamp - captchaTimestamp
                                                  < captchaExpireTime)
                                              captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'


captchaExpireTime :: Int64
captchaExpireTime = 60*60*1

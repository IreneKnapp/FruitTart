module Network.FruitTart.Custard.Functions.Captchas where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.ByteString hiding (map, concat, index)
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
                  -> FruitTart CustardValue
cfGenerateCaptcha context parameters = do
  requireControllerContext context "generateCaptcha"
  requireNParameters parameters 0 "generateCaptcha"
  expireOldCaptchas
  captchaCacheMVar <- getCaptchaCacheMVar
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  timestamp <- getTimestamp
  (string, byteString) <- liftIO $ makeCaptcha
  captchaCache' <- return $ Map.insert timestamp (string, byteString) captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return $ CustardInteger timestamp


cfCheckCaptcha :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfCheckCaptcha context parameters = do
  requireControllerContext context "checkCaptcha"
  requireNParameters parameters 2 "checkCaptcha"
  timestamp <- valueToInteger $ parameters !! 0
  responseString <- valueToString $ parameters !! 1
  expireOldCaptchas
  captchaCacheMVar <- getCaptchaCacheMVar
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captcha <- return $ Map.lookup timestamp captchaCache
  captchaCache' <- return $ Map.delete timestamp captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return $ CustardBool $ case captcha of
    Nothing -> False
    Just (challengeString, _) -> (map toUpper responseString) == challengeString


cfExpireOldCaptchas :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfExpireOldCaptchas context parameters = do
  requireControllerContext context "expireOldCaptchas"
  requireNParameters parameters 0 "expireOldCaptchas"
  expireOldCaptchas
  return $ CustardNull


expireOldCaptchas :: FruitTart ()
expireOldCaptchas = do
  currentTimestamp <- getTimestamp
  captchaCacheMVar <- getCaptchaCacheMVar
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captchaCache' <- return $ Map.filterWithKey (\captchaTimestamp _
                                               -> currentTimestamp - captchaTimestamp
                                                  < captchaExpireTime)
                                              captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'


captchaExpireTime :: Int64
captchaExpireTime = 60*60*1

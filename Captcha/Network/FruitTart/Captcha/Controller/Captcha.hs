module Network.FruitTart.Captcha.Controller.Captcha
    (actionTable, generateCaptcha, checkCaptcha) where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map, concat, index)
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List

import Graphics.Captcha

import Network.FruitTart.Base
import Network.FruitTart.Util


actionTable :: ActionTable
actionTable
    = makeActionTable [("index", "GET", [IDParameter], [], [], toDyn index)]


index :: Int64 -> FruitTart ()
index timestamp = do
  expireOldCaptchas
  captchaCacheMVar <- getInterfaceStateMVar "Captcha"
                   :: FruitTart (MVar (Map Int64 (String, ByteString)))
  captchaCache <- liftIO $ readMVar captchaCacheMVar
  maybeCaptcha <- return $ Map.lookup timestamp captchaCache
  case maybeCaptcha of
    Nothing -> do
      error404 "Captcha has expired; try again."
    Just (_, byteString) -> do
                      setResponseHeader HttpContentType "image/png"
                      fPut byteString


generateCaptcha :: FruitTart Int64
generateCaptcha = do
  expireOldCaptchas
  captchaCacheMVar <- getInterfaceStateMVar "Captcha"
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  timestamp <- getTimestamp
  (string, byteString) <- liftIO $ makeCaptcha
  captchaCache' <- return $ Map.insert timestamp (string, byteString) captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return timestamp


checkCaptcha :: Int64 -> String -> FruitTart Bool
checkCaptcha timestamp responseString = do
  expireOldCaptchas
  captchaCacheMVar <- getInterfaceStateMVar "Captcha"
                   :: FruitTart (MVar (Map Int64 (String, ByteString)))
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captcha <- return $ Map.lookup timestamp captchaCache
  captchaCache' <- return $ Map.delete timestamp captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  case captcha of
    Nothing -> return False
    Just (challengeString, _) -> return $ (map toUpper responseString)
                                          == challengeString


expireOldCaptchas :: FruitTart ()
expireOldCaptchas = do
  currentTimestamp <- getTimestamp
  captchaCacheMVar <- getInterfaceStateMVar "Captcha"
                   :: FruitTart (MVar (Map Int64 (String, ByteString)))
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captchaCache' <- return $ Map.filterWithKey (\captchaTimestamp _
                                               -> currentTimestamp - captchaTimestamp
                                                  < captchaExpireTime)
                                              captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'


captchaExpireTime :: Int64
captchaExpireTime = 60*60*1

module Controller.Captcha where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString hiding (map)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.List
import Network.FastCGI

import Graphics.Captcha

import {-# SOURCE #-} Buglist
import Database
import Dispatcher
import HTML
import Lists
import SQLite3 (SQLData(..))
import Types


index :: Int64 -> Buglist CGIResult
index timestamp = do
  expireOldCaptchas
  BuglistState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ readMVar captchaCacheMVar
  maybeCaptcha <- return $ Map.lookup timestamp captchaCache
  case maybeCaptcha of
    Nothing -> do
      error404 "Captcha has expired; try again."
    Just (_, byteString) -> do
                      lazyByteString <- return $ Lazy.pack $ unpack byteString
                      setHeader "Content-Type" "image/png"
                      outputFPS lazyByteString


generateCaptcha :: Buglist Int64
generateCaptcha = do
  expireOldCaptchas
  BuglistState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  timestamp <- getTimestamp
  (string, byteString) <- liftIO $ makeCaptcha
  captchaCache' <- return $ Map.insert timestamp (string, byteString) captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  return timestamp


checkCaptcha :: Int64 -> String -> Buglist Bool
checkCaptcha timestamp responseString = do
  expireOldCaptchas
  BuglistState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captcha <- return $ Map.lookup timestamp captchaCache
  captchaCache' <- return $ Map.delete timestamp captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'
  case captcha of
    Nothing -> return False
    Just (challengeString, _) -> return $ (map toUpper responseString)
                                          == challengeString


expireOldCaptchas :: Buglist ()
expireOldCaptchas = do
  currentTimestamp <- getTimestamp
  BuglistState { captchaCacheMVar = captchaCacheMVar } <- get
  captchaCache <- liftIO $ takeMVar captchaCacheMVar
  captchaCache' <- return $ Map.filterWithKey (\captchaTimestamp _
                                               -> currentTimestamp - captchaTimestamp
                                                  < captchaExpireTime)
                                              captchaCache
  liftIO $ putMVar captchaCacheMVar captchaCache'


captchaExpireTime :: Int64
captchaExpireTime = 60*60*1

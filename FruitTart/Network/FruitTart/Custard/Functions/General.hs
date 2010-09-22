module Network.FruitTart.Custard.Functions.General (
                                                    cfJust,
                                                    cfParameter,
                                                    cfIsNothing,
                                                    cfIsJust,
                                                    cfFromJust,
                                                    cfCompareIntegers,
                                                    cfShowInteger,
                                                    cfShowBool,
                                                    cfByteSizeToString,
                                                    cfTimestampToString,
                                                    cfGetCurrentPage,
                                                    cfGetController,
                                                    cfIdentity,
                                                    cfEval
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

import {-# SOURCE #-} Network.FruitTart.Custard.Semantics
import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfJust :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfJust context parameters = do
  requireNParameters parameters 1 "just"
  return (context, CustardMaybe $ Just $ head parameters)


cfParameter :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfParameter context parameters = do
  requireNParameters parameters 1 "parameter"
  n <- valueToInteger $ head parameters
  let CustardContext { custardContextParameters = contextParameters } = context
  if n < (fromIntegral $ length contextParameters)
    then return (context, head $ drop (fromIntegral n) contextParameters)
    else error $ "Too few parameters for parameter(" ++ (show n) ++ ")."


cfIsNothing :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfIsNothing context parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  case value of
    CustardMaybe Nothing -> return (context, CustardBool True)
    CustardMaybe (Just _) -> return (context, CustardBool False)
    _ -> error $ "Parameter is not a Maybe in isNothing()."


cfIsJust :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfIsJust context parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  case value of
    CustardMaybe Nothing -> return (context, CustardBool False)
    CustardMaybe (Just _) -> return (context, CustardBool True)
    _ -> error $ "Parameter is not a Maybe in isJust()."


cfFromJust :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfFromJust context parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  case value of
    CustardMaybe Nothing
      -> error $ "Parameter is nothing in fromJust()."
    CustardMaybe (Just result) -> return (context, result)
    _ -> error $ "Parameter is not a Maybe in fromJust()."


cfCompareIntegers :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfCompareIntegers context parameters = do
  requireNParameters parameters 2 "compareIntegers"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return (context, CustardOrdering $ compare a b)


cfShowInteger :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfShowInteger context parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return (context, CustardString $ UTF8.fromString $ show integer)


cfShowBool :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfShowBool context parameters = do
  requireNParameters parameters 1 "showBool"
  bool <- valueToBoolean $ head parameters
  return (context, CustardString $ UTF8.fromString $ show bool)


cfByteSizeToString :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfByteSizeToString context parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return (context, CustardString $ UTF8.fromString $ byteSizeToString integer)


cfTimestampToString :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfTimestampToString context parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return (context, CustardString $ UTF8.fromString $ timestampToString integer)


cfGetCurrentPage :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfGetCurrentPage context parameters = do
  requireControllerContext context "getCurrentPage"
  requireNParameters parameters 0 "getCurrentPage"
  FruitTartState { maybeCurrentPage = maybeURL } <- get
  case maybeURL of
    Nothing -> error "Not on a page."
    Just url -> return (context, CustardString $ UTF8.fromString url)


cfGetController :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfGetController context parameters = do
  requireControllerContext context "getController"
  requireNParameters parameters 0 "getController"
  FruitTartState { maybeControllerName = maybeControllerName } <- get
  case maybeControllerName of
    Nothing -> error "Not in a controller."
    Just controllerName -> return (context,
                                   CustardString
                                    $ UTF8.fromString controllerName)


cfIdentity :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfIdentity context parameters = do
  requireNParameters parameters 1 "identity"
  return (context, parameters !! 0)


cfEval :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfEval context parameters = do
  requireControllerContext context "eval"
  requireNParameters parameters 1 "eval"
  bytestring <- valueToUTF8String $ parameters !! 0
  result <- eval "Base" (UTF8.toString bytestring)
  return (context, result)

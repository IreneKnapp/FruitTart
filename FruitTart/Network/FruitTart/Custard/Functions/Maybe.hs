module Network.FruitTart.Custard.Functions.Maybe (
                                                  cfJust,
                                                  cfMaybe,
                                                  cfIsJust,
                                                  cfIsNothing,
                                                  cfFromJust,
                                                  cfFromMaybe,
                                                  cfListToMaybe,
                                                  cfMaybeToList,
                                                  cfCatMaybes,
                                                  cfMapMaybe
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
  requireNParameters parameters 1 "Just"
  return (context, CustardMaybe $ Just $ head parameters)


cfMaybe :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfMaybe context parameters = do
  error "Not yet implemented."
  -- TODO


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


cfFromJust :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfFromJust context parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  case value of
    CustardMaybe Nothing
      -> error $ "Parameter is Nothing in fromJust()."
    CustardMaybe (Just result) -> return (context, result)
    _ -> error $ "Parameter is not a Maybe in fromJust()."


cfFromMaybe :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfFromMaybe context parameters = do
  error "Not yet implemented."
  -- TODO


cfListToMaybe :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfListToMaybe context parameters = do
  error "Not yet implemented."
  -- TODO


cfMaybeToList :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMaybeToList context parameters = do
  error "Not yet implemented."
  -- TODO


cfCatMaybes :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfCatMaybes context parameters = do
  error "Not yet implemented."
  -- TODO


cfMapMaybe :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapMaybe context parameters = do
  error "Not yet implemented."
  -- TODO

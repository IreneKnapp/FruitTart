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
  requireNParameters parameters 2 "maybe"
  defaultValue <- return $ parameters !! 0
  action <- valueToMonadicAction $ parameters !! 1
  maybeValue <- return $ parameters !! 2
  case maybeValue of
    CustardMaybe Nothing -> return (context, defaultValue)
    CustardMaybe (Just actualValue) -> action context [actualValue]
    _ -> error $ "Parameter is not a Maybe in maybe()."


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
  requireNParameters parameters 2 "fromMaybe"
  defaultValue <- return $ parameters !! 0
  maybeValue <- return $ parameters !! 1
  case maybeValue of
    CustardMaybe Nothing -> return (context, defaultValue)
    CustardMaybe (Just actualValue) -> return (context, actualValue)
    _ -> error $ "Parameter is not a Maybe in fromMaybe()."


cfListToMaybe :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfListToMaybe context parameters = do
  requireNParameters parameters 1 "listToMaybe"
  value <- return $ parameters !! 0
  case value of
    CustardList [] -> return (context, CustardMaybe Nothing)
    CustardList (head:_) -> return (context, CustardMaybe $ Just head)
    _ -> error $ "Parameter is not a List in listToMaybe()."


cfMaybeToList :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMaybeToList context parameters = do
  requireNParameters parameters 1 "maybeToList"
  value <- return $ parameters !! 0
  case value of
    CustardMaybe Nothing
      -> return (context, CustardList [])
    CustardMaybe (Just result) -> return (context, CustardList [result])
    _ -> error $ "Parameter is not a Maybe in maybeToList()."


cfCatMaybes :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfCatMaybes context parameters = do
  requireNParameters parameters 1 "catMaybes"
  value <- return $ parameters !! 0
  case value of
    (CustardList []) -> return (context, CustardList [])
    (CustardList items@(CustardMaybe _ : _)) ->
      return (context,
              CustardList
               $ foldl (\result maybeValue ->
                          case maybeValue of
                            CustardMaybe Nothing -> result
                            CustardMaybe (Just something) -> result ++ [something])
                       []
                       items)
    _ -> error "Parameter is not a List of Maybes in catMaybes()."


cfMapMaybe :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapMaybe context parameters = do
  requireNParameters parameters 2 "mapMaybe"
  action <- valueToMonadicAction $ parameters !! 0
  items <- return $ parameters !! 1
  case items of
    (CustardList items) -> do
       (context, result)
         <- foldM (\(context, result) item -> do
                     (context, itemResult) <- action context [item]
                     case itemResult of
                       CustardMaybe Nothing -> return (context, result)
                       CustardMaybe (Just something) ->
                         return (context, result ++ [something])
                       _ -> error "Function result is not a Maybe in mapMaybe().")
                  (context, [])
                  items
       return (context, CustardList result)
    _ -> error "Parameter is not a List in mapMaybe()."

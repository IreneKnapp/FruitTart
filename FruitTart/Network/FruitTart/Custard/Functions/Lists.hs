{-# LANGUAGE GADTs #-}
module Network.FruitTart.Custard.Functions.Lists (
                                                  cfHead,
                                                  cfLast,
                                                  cfTail,
                                                  cfInit,
                                                  cfNull,
                                                  cfLength,
                                                  cfMap,
                                                  cfReverse,
                                                  cfIntersperse,
                                                  cfIntercalate,
                                                  cfTranspose,
                                                  cfSubsequences,
                                                  cfPermutations,
                                                  cfFold,
                                                  cfFold1,
                                                  cfConcat,
                                                  cfConcatMap,
                                                  cfAnd,
                                                  cfOr,
                                                  cfAny,
                                                  cfAll,
                                                  cfSum,
                                                  cfProduct,
                                                  cfMaximum,
                                                  cfMinimum,
                                                  cfScanl,
                                                  cfScanl1,
                                                  cfScanr,
                                                  cfScanr1,
                                                  cfMapAccumL,
                                                  cfMapAccumR,
                                                  cfReplicate,
                                                  cfUnfoldr,
                                                  cfTake,
                                                  cfDrop,
                                                  cfSplitAt,
                                                  cfTakeWhile,
                                                  cfDropWhile,
                                                  cfSpan,
                                                  cfBreak,
                                                  cfStripPrefix,
                                                  cfGroup,
                                                  cfInits,
                                                  cfTails,
                                                  cfIsPrefixOf,
                                                  cfIsSuffixOf,
                                                  cfIsInfixOf,
                                                  cfElem,
                                                  cfNotElem,
                                                  cfLookup,
                                                  cfFind,
                                                  cfFilter,
                                                  cfPartition,
                                                  cfNth,
                                                  cfElemIndex,
                                                  cfElemIndices,
                                                  cfFindIndex,
                                                  cfFindIndices,
                                                  cfNub,
                                                  cfDelete,
                                                  cfDeleteFirsts,
                                                  cfUnion,
                                                  cfIntersect,
                                                  cfSort,
                                                  cfInsert,
                                                  cfNubBy,
                                                  cfDeleteBy,
                                                  cfDeleteFirstsBy,
                                                  cfUnionBy,
                                                  cfIntersectBy,
                                                  cfGroupBy,
                                                  cfSortBy,
                                                  cfInsertBy,
                                                  cfMaximumBy,
                                                  cfMinimumBy
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

import Network.FruitTart.Custard.Syntax
import {-# SOURCE #-} Network.FruitTart.Custard.Semantics
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfHead :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfHead context parameters = do
  requireNParameters parameters 1 "head"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return (context, head items)
    _ -> errorNotAList


cfLast :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfLast context parameters = do
  requireNParameters parameters 1 "last"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return (context, last items)
    _ -> errorNotAList


cfTail :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfTail context parameters = do
  requireNParameters parameters 1 "tail"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return (context, CustardList $ tail items)
    _ -> errorNotAList


cfInit :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfInit context parameters = do
  requireNParameters parameters 1 "init"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return (context, CustardList $ init items)
    _ -> errorNotAList


cfNull :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfNull context parameters = do
  requireNParameters parameters 1 "null"
  case head parameters of
    CustardList items -> return (context, CustardBool $ null items)
    _ -> errorNotAList


cfLength :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfLength context parameters = do
  requireNParameters parameters 1 "length"
  case head parameters of
    CustardList items -> return (context,
                                 CustardInteger $ fromIntegral $ length items)
    _ -> errorNotAList


cfMap :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfMap context parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  case head $ drop 1 parameters of
    CustardList items -> do
      (context, items) <- foldM (\(context, results) a -> do
                                   (context, result)
                                     <- applyFunctionGivenContextAndValue
                                         context function [a]
                                   return (context, results ++ [result]))
                                (context, [])
                                items
      typecheckList items
      return (context, CustardList items)
    _ -> errorNotAList


cfReverse :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfReverse context parameters = do
  requireNParameters parameters 1 "reverse"
  case head parameters of
    CustardList items -> return (context, CustardList $ reverse items)
    _ -> errorNotAList


cfIntersperse :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfIntersperse context parameters = do
  error "Not implemented."
  -- TODO


cfIntercalate :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfIntercalate context parameters = do
  error "Not implemented."
  -- TODO


cfTranspose :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfTranspose context parameters = do
  error "Not implemented."
  -- TODO


cfSubsequences :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfSubsequences context parameters = do
  error "Not implemented."
  -- TODO


cfPermutations :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfPermutations context parameters = do
  error "Not implemented."
  -- TODO


cfFold :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfFold context parameters = do
  error "Not implemented."
  -- TODO


cfFold1 :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfFold1 context parameters = do
  error "Not implemented."
  -- TODO


cfConcat :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfConcat context parameters = do
  error "Not implemented."
  -- TODO


cfConcatMap :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfAnd :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfAnd context parameters = do
  error "Not implemented."
  -- TODO


cfOr :: CustardContext
     -> [CustardValue]
     -> FruitTart (CustardContext, CustardValue)
cfOr context parameters = do
  error "Not implemented."
  -- TODO


cfAny :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfAny context parameters = do
  error "Not implemented."
  -- TODO


cfAll :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfAll context parameters = do
  error "Not implemented."
  -- TODO


cfSum :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfSum context parameters = do
  error "Not implemented."
  -- TODO


cfProduct :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfProduct context parameters = do
  error "Not implemented."
  -- TODO


cfMaximum :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMaximum context parameters = do
  error "Not implemented."
  -- TODO


cfMinimum :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMinimum context parameters = do
  error "Not implemented."
  -- TODO


cfScanl :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfScanl context parameters = do
  error "Not implemented."
  -- TODO


cfScanl1 :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfScanr :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfScanr context parameters = do
  error "Not implemented."
  -- TODO


cfScanr1 :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumL :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumR :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfReplicate :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfReplicate context parameters = do
  error "Not implemented."
  -- TODO


cfUnfoldr :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfTake :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfTake context parameters = do
  error "Not implemented."
  -- TODO


cfDrop :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfDrop context parameters = do
  error "Not implemented."
  -- TODO


cfSplitAt :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfSplitAt context parameters = do
  error "Not implemented."
  -- TODO


cfTakeWhile :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfDropWhile :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfSpan :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfSpan context parameters = do
  error "Not implemented."
  -- TODO


cfBreak :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStripPrefix :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStripPrefix context parameters = do
  error "Not implemented."
  -- TODO


cfGroup :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfGroup context parameters = do
  error "Not implemented."
  -- TODO


cfInits :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfInits context parameters = do
  error "Not implemented."
  -- TODO


cfTails :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfTails context parameters = do
  error "Not implemented."
  -- TODO


cfIsPrefixOf :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfIsPrefixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsSuffixOf :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfIsSuffixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsInfixOf :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfIsInfixOf context parameters = do
  error "Not implemented."
  -- TODO


cfElem :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfElem context parameters = do
  error "Not implemented."
  -- TODO


cfNotElem :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfNotElem context parameters = do
  error "Not implemented."
  -- TODO


cfLookup :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfLookup context parameters = do
  error "Not implemented."
  -- TODO


cfFind :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfFind context parameters = do
  error "Not implemented."
  -- TODO


cfFilter :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfFilter context parameters = do
  error "Not implemented."
  -- TODO


cfPartition :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfPartition context parameters = do
  error "Not implemented."
  -- TODO


cfNth :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfNth context parameters = do
  requireNParameters parameters 2 "nth"
  n <- valueToInteger $ head parameters
  case head $ drop 1 parameters of
    CustardList items -> return (context, items !! fromIntegral n)
    _ -> errorNotAList


cfElemIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfElemIndex context parameters = do
  error "Not implemented."
  -- TODO


cfElemIndices :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfElemIndices context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndices :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfNub :: CustardContext
      -> [CustardValue]
      -> FruitTart (CustardContext, CustardValue)
cfNub context parameters = do
  error "Not implemented."
  -- TODO


cfDelete :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfDelete context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirsts :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDeleteFirsts context parameters = do
  error "Not implemented."
  -- TODO


cfUnion :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfUnion context parameters = do
  error "Not implemented."
  -- TODO


cfIntersect :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfIntersect context parameters = do
  error "Not implemented."
  -- TODO


cfSort :: CustardContext
       -> [CustardValue]
       -> FruitTart (CustardContext, CustardValue)
cfSort context parameters = do
  error "Not implemented."
  -- TODO


cfInsert :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfInsert context parameters = do
  error "Not implemented."
  -- TODO


cfNubBy :: CustardContext
        -> [CustardValue]
        -> FruitTart (CustardContext, CustardValue)
cfNubBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteBy :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDeleteBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirstsBy :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDeleteFirstsBy context parameters = do
  error "Not implemented."
  -- TODO


cfUnionBy :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfUnionBy context parameters = do
  error "Not implemented."
  -- TODO


cfIntersectBy :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfIntersectBy context parameters = do
  error "Not implemented."
  -- TODO


cfGroupBy :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfGroupBy context parameters = do
  error "Not implemented."
  -- TODO


cfSortBy :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfSortBy context parameters = do
  error "Not implemented."
  -- TODO


cfInsertBy :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfInsertBy context parameters = do
  error "Not implemented."
  -- TODO


cfMaximumBy :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMaximumBy context parameters = do
  error "Not implemented."
  -- TODO


cfMinimumBy :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMinimumBy context parameters = do
  error "Not implemented."
  -- TODO

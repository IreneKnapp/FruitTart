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
                                                  cfFoldl,
                                                  cfFoldl1,
                                                  cfFoldr,
                                                  cfFoldr1,
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
import Network.FruitTart.Common
import Network.FruitTart.Types
import Network.FruitTart.Util


cfHead :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfHead context parameters = do
  requireNParameters parameters 1 "head"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return $ head items
    _ -> errorNotAList


cfLast :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfLast context parameters = do
  requireNParameters parameters 1 "last"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return $ last items
    _ -> errorNotAList


cfTail :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfTail context parameters = do
  requireNParameters parameters 1 "tail"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return $ CustardList $ tail items
    _ -> errorNotAList


cfInit :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfInit context parameters = do
  requireNParameters parameters 1 "init"
  case head parameters of
    CustardList [] -> errorListEmpty
    CustardList items -> return $ CustardList $ init items
    _ -> errorNotAList


cfNull :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfNull context parameters = do
  requireNParameters parameters 1 "null"
  case head parameters of
    CustardList items -> return $ CustardBool $ null items
    _ -> errorNotAList


cfLength :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfLength context parameters = do
  requireNParameters parameters 1 "length"
  case head parameters of
    CustardList items -> return $ CustardInteger $ fromIntegral $ length items
    _ -> errorNotAList


cfMap :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfMap context parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  case head $ drop 1 parameters of
    CustardList items -> do
      (_, items) <- foldM (\(context, results) a -> do
                            (context, result)
                              <- applyFunctionGivenContextAndValue
                                  context function [a]
                            return (context, results ++ [result]))
                         (context, [])
                         items
      typecheckList items
      return $ CustardList items
    _ -> errorNotAList


cfReverse :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfReverse context parameters = do
  requireNParameters parameters 1 "reverse"
  case head parameters of
    CustardList items -> return $ CustardList $ reverse items
    _ -> errorNotAList


cfIntersperse :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfIntersperse context parameters = do
  error "Not implemented."
  -- TODO


cfIntercalate :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfIntercalate context parameters = do
  error "Not implemented."
  -- TODO


cfTranspose :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfTranspose context parameters = do
  error "Not implemented."
  -- TODO


cfSubsequences :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfSubsequences context parameters = do
  error "Not implemented."
  -- TODO


cfPermutations :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfPermutations context parameters = do
  error "Not implemented."
  -- TODO


cfFoldl :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfFoldl context parameters = do
  error "Not implemented."
  -- TODO


cfFoldl1 :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfFoldl1 context parameters = do
  error "Not implemented."
  -- TODO


cfFoldr :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfFoldr context parameters = do
  error "Not implemented."
  -- TODO


cfFoldr1 :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfFoldr1 context parameters = do
  error "Not implemented."
  -- TODO


cfConcat :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfConcat context parameters = do
  error "Not implemented."
  -- TODO


cfConcatMap :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfAnd :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfAnd context parameters = do
  error "Not implemented."
  -- TODO


cfOr :: CustardContext
     -> [CustardValue]
     -> FruitTart CustardValue
cfOr context parameters = do
  error "Not implemented."
  -- TODO


cfAny :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfAny context parameters = do
  error "Not implemented."
  -- TODO


cfAll :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfAll context parameters = do
  error "Not implemented."
  -- TODO


cfSum :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfSum context parameters = do
  error "Not implemented."
  -- TODO


cfProduct :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfProduct context parameters = do
  error "Not implemented."
  -- TODO


cfMaximum :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMaximum context parameters = do
  error "Not implemented."
  -- TODO


cfMinimum :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMinimum context parameters = do
  error "Not implemented."
  -- TODO


cfScanl :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfScanl context parameters = do
  error "Not implemented."
  -- TODO


cfScanl1 :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfScanr :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfScanr context parameters = do
  error "Not implemented."
  -- TODO


cfScanr1 :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumL :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumR :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfReplicate :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfReplicate context parameters = do
  error "Not implemented."
  -- TODO


cfUnfoldr :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfTake :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfTake context parameters = do
  error "Not implemented."
  -- TODO


cfDrop :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfDrop context parameters = do
  error "Not implemented."
  -- TODO


cfSplitAt :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfSplitAt context parameters = do
  error "Not implemented."
  -- TODO


cfTakeWhile :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfDropWhile :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfSpan :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfSpan context parameters = do
  error "Not implemented."
  -- TODO


cfBreak :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStripPrefix :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStripPrefix context parameters = do
  error "Not implemented."
  -- TODO


cfGroup :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfGroup context parameters = do
  error "Not implemented."
  -- TODO


cfInits :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfInits context parameters = do
  error "Not implemented."
  -- TODO


cfTails :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfTails context parameters = do
  error "Not implemented."
  -- TODO


cfIsPrefixOf :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfIsPrefixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsSuffixOf :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfIsSuffixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsInfixOf :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfIsInfixOf context parameters = do
  error "Not implemented."
  -- TODO


cfElem :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfElem context parameters = do
  error "Not implemented."
  -- TODO


cfNotElem :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfNotElem context parameters = do
  error "Not implemented."
  -- TODO


cfLookup :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfLookup context parameters = do
  error "Not implemented."
  -- TODO


cfFind :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfFind context parameters = do
  error "Not implemented."
  -- TODO


cfFilter :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfFilter context parameters = do
  error "Not implemented."
  -- TODO


cfPartition :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfPartition context parameters = do
  error "Not implemented."
  -- TODO


cfNth :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfNth context parameters = do
  requireNParameters parameters 2 "nth"
  n <- valueToInteger $ head parameters
  case head $ drop 1 parameters of
    CustardList items -> return $ items !! fromIntegral n
    _ -> errorNotAList


cfElemIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfElemIndex context parameters = do
  error "Not implemented."
  -- TODO


cfElemIndices :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfElemIndices context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndices :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfNub :: CustardContext
      -> [CustardValue]
      -> FruitTart CustardValue
cfNub context parameters = do
  error "Not implemented."
  -- TODO


cfDelete :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfDelete context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirsts :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDeleteFirsts context parameters = do
  error "Not implemented."
  -- TODO


cfUnion :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfUnion context parameters = do
  error "Not implemented."
  -- TODO


cfIntersect :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfIntersect context parameters = do
  error "Not implemented."
  -- TODO


cfSort :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfSort context parameters = do
  error "Not implemented."
  -- TODO


cfInsert :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfInsert context parameters = do
  error "Not implemented."
  -- TODO


cfNubBy :: CustardContext
        -> [CustardValue]
        -> FruitTart CustardValue
cfNubBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteBy :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDeleteBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirstsBy :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDeleteFirstsBy context parameters = do
  error "Not implemented."
  -- TODO


cfUnionBy :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfUnionBy context parameters = do
  error "Not implemented."
  -- TODO


cfIntersectBy :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfIntersectBy context parameters = do
  error "Not implemented."
  -- TODO


cfGroupBy :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfGroupBy context parameters = do
  error "Not implemented."
  -- TODO


cfSortBy :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfSortBy context parameters = do
  error "Not implemented."
  -- TODO


cfInsertBy :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfInsertBy context parameters = do
  error "Not implemented."
  -- TODO


cfMaximumBy :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMaximumBy context parameters = do
  error "Not implemented."
  -- TODO


cfMinimumBy :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMinimumBy context parameters = do
  error "Not implemented."
  -- TODO

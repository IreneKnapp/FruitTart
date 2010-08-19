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
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfHead context parameters = do
  requireNParameters parameters 1 "head"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items) -> return $ SomeCustardValue $ head items
    _ -> errorNotAList


cfLast :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfLast context parameters = do
  requireNParameters parameters 1 "last"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items) -> return $ SomeCustardValue $ last items
    _ -> errorNotAList


cfTail :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfTail context parameters = do
  requireNParameters parameters 1 "tail"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ CustardList $ tail items
    _ -> errorNotAList


cfInit :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfInit context parameters = do
  requireNParameters parameters 1 "init"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ CustardList $ init items
    _ -> errorNotAList


cfNull :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfNull context parameters = do
  requireNParameters parameters 1 "null"
  case head parameters of
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ CustardBool $ null items
    _ -> errorNotAList


cfLength :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfLength context parameters = do
  requireNParameters parameters 1 "length"
  case head parameters of
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ CustardInteger $ fromIntegral $ length items
    _ -> errorNotAList


cfMap :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfMap context parameters = do
  requireNParameters parameters 2 "map"
  let function = head parameters
  case head $ drop 1 parameters of
    SomeCustardValue (CustardList items) -> do
      (_, items) <- foldM (\(context, results) a -> do
                            (context, result)
                              <- applyFunctionGivenContextAndValue
                                  context function [SomeCustardValue a]
                            return (context, results ++ [result]))
                         (context, [])
                         items
      typecheckAndHomogenizeList items
    _ -> errorNotAList


cfReverse :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfReverse context parameters = do
  requireNParameters parameters 1 "reverse"
  case head parameters of
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ CustardList $ reverse items
    _ -> errorNotAList


cfIntersperse :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfIntersperse context parameters = do
  error "Not implemented."
  -- TODO


cfIntercalate :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfIntercalate context parameters = do
  error "Not implemented."
  -- TODO


cfTranspose :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfTranspose context parameters = do
  error "Not implemented."
  -- TODO


cfSubsequences :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfSubsequences context parameters = do
  error "Not implemented."
  -- TODO


cfPermutations :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfPermutations context parameters = do
  error "Not implemented."
  -- TODO


cfFoldl :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfFoldl context parameters = do
  error "Not implemented."
  -- TODO


cfFoldl1 :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfFoldl1 context parameters = do
  error "Not implemented."
  -- TODO


cfFoldr :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfFoldr context parameters = do
  error "Not implemented."
  -- TODO


cfFoldr1 :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfFoldr1 context parameters = do
  error "Not implemented."
  -- TODO


cfConcat :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfConcat context parameters = do
  requireNParameters parameters 1 "concat"
  case head parameters of
    SomeCustardValue (CustardList items@(CustardList _ : _))
      -> return $ SomeCustardValue
                $ CustardList
                $ concat
                $ map (\(CustardList sublist) -> sublist) items
    _ -> errorNotAListOfLists


cfConcatMap :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfAnd :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfAnd context parameters = do
  requireNParameters parameters 1 "and"
  case head parameters of
    SomeCustardValue (CustardList [])
      -> return $ SomeCustardValue $ CustardBool $ and []
    SomeCustardValue (CustardList items@(CustardBool _ : _))
      -> return $ SomeCustardValue
                $ CustardBool
                $ and
                $ map (\(CustardBool item) -> item) items
    _ -> errorNotAListOfBooleans


cfOr :: CustardContext
     -> [AnyCustardValue]
     -> FruitTart AnyCustardValue
cfOr context parameters = do
  requireNParameters parameters 1 "or"
  case head parameters of
    SomeCustardValue (CustardList [])
      -> return $ SomeCustardValue $ CustardBool $ and []
    SomeCustardValue (CustardList items@(CustardBool _ : _))
      -> return $ SomeCustardValue
                $ CustardBool
                $ or
                $ map (\(CustardBool item) -> item) items
    _ -> errorNotAListOfBooleans


cfAny :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfAny context parameters = do
  error "Not implemented."
  -- TODO


cfAll :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfAll context parameters = do
  error "Not implemented."
  -- TODO


cfSum :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfSum context parameters = do
  requireNParameters parameters 1 "sum"
  case head parameters of
    SomeCustardValue (CustardList [])
      -> return $ SomeCustardValue $ CustardInteger $ sum []
    SomeCustardValue (CustardList items@(CustardInteger _ : _))
      -> return $ SomeCustardValue
                $ CustardBool
                $ sum
                $ map (\(CustardInteger item) -> item) items
    _ -> errorNotAListOfIntegers


cfProduct :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfProduct context parameters = do
  requireNParameters parameters 1 "product"
  case head parameters of
    SomeCustardValue (CustardList [])
      -> return $ SomeCustardValue $ CustardInteger $ product []
    SomeCustardValue (CustardList items@(CustardInteger _ : _))
      -> return $ SomeCustardValue
                $ CustardBool
                $ product
                $ map (\(CustardInteger item) -> item) items
    _ -> errorNotAListOfIntegers


cfMaximum :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfMaximum context parameters = do
  requireNParameters parameters 1 "maximum"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items@(CustardInteger _ : _))
      -> mapM valueToInteger items
         >>= return . SomeCustardValue . CustardInteger . maximum
    SomeCustardValue (CustardList items@(CustardCharacter _ : _))
      -> mapM valueToCharacter items
         >>= return . SomeCustardValue . CustardCharacter . maximum
    _ -> errorNotAListOfIntegersOrCharacters


cfMinimum :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfMinimum context parameters = do
  requireNParameters parameters 1 "minimum"
  case head parameters of
    SomeCustardValue (CustardList []) -> errorListEmpty
    SomeCustardValue (CustardList items@(CustardInteger _ : _))
      -> mapM valueToInteger items
         >>= return . SomeCustardValue . CustardInteger . minimum
    SomeCustardValue (CustardList items@(CustardCharacter _ : _))
      -> mapM valueToCharacter items
         >>= return . SomeCustardValue . CustardCharacter . minimum
    _ -> errorNotAListOfIntegersOrCharacters


cfScanl :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfScanl context parameters = do
  error "Not implemented."
  -- TODO


cfScanl1 :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfScanr :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfScanr context parameters = do
  error "Not implemented."
  -- TODO


cfScanr1 :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumL :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfMapAccumR :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfReplicate :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfReplicate context parameters = do
  error "Not implemented."
  -- TODO


cfUnfoldr :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfTake :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfTake context parameters = do
  error "Not implemented."
  -- TODO


cfDrop :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfDrop context parameters = do
  error "Not implemented."
  -- TODO


cfSplitAt :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfSplitAt context parameters = do
  error "Not implemented."
  -- TODO


cfTakeWhile :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfDropWhile :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfSpan :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfSpan context parameters = do
  error "Not implemented."
  -- TODO


cfBreak :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStripPrefix :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStripPrefix context parameters = do
  error "Not implemented."
  -- TODO


cfGroup :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfGroup context parameters = do
  error "Not implemented."
  -- TODO


cfInits :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfInits context parameters = do
  error "Not implemented."
  -- TODO


cfTails :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfTails context parameters = do
  error "Not implemented."
  -- TODO


cfIsPrefixOf :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfIsPrefixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsSuffixOf :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfIsSuffixOf context parameters = do
  error "Not implemented."
  -- TODO


cfIsInfixOf :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfIsInfixOf context parameters = do
  error "Not implemented."
  -- TODO


cfElem :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfElem context parameters = do
  error "Not implemented."
  -- TODO


cfNotElem :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfNotElem context parameters = do
  error "Not implemented."
  -- TODO


cfLookup :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfLookup context parameters = do
  error "Not implemented."
  -- TODO


cfFind :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfFind context parameters = do
  error "Not implemented."
  -- TODO


cfFilter :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfFilter context parameters = do
  error "Not implemented."
  -- TODO


cfPartition :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfPartition context parameters = do
  error "Not implemented."
  -- TODO


cfNth :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfNth context parameters = do
  requireNParameters parameters 2 "nth"
  n <- valueToInteger $ head parameters
  case head $ drop 1 parameters of
    SomeCustardValue (CustardList items)
      -> return $ SomeCustardValue $ items !! n
    _ -> errorNotAList


cfElemIndex :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfElemIndex context parameters = do
  error "Not implemented."
  -- TODO


cfElemIndices :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfElemIndices context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndex :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfFindIndices :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfNub :: CustardContext
      -> [AnyCustardValue]
      -> FruitTart AnyCustardValue
cfNub context parameters = do
  error "Not implemented."
  -- TODO


cfDelete :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfDelete context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirsts :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfDeleteFirsts context parameters = do
  error "Not implemented."
  -- TODO


cfUnion :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfUnion context parameters = do
  error "Not implemented."
  -- TODO


cfIntersect :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfIntersect context parameters = do
  error "Not implemented."
  -- TODO


cfSort :: CustardContext
       -> [AnyCustardValue]
       -> FruitTart AnyCustardValue
cfSort context parameters = do
  error "Not implemented."
  -- TODO


cfInsert :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfInsert context parameters = do
  error "Not implemented."
  -- TODO


cfNubBy :: CustardContext
        -> [AnyCustardValue]
        -> FruitTart AnyCustardValue
cfNubBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteBy :: CustardContext
           -> [AnyCustardValue]
           -> FruitTart AnyCustardValue
cfDeleteBy context parameters = do
  error "Not implemented."
  -- TODO


cfDeleteFirstsBy :: CustardContext
                 -> [AnyCustardValue]
                 -> FruitTart AnyCustardValue
cfDeleteFirstsBy context parameters = do
  error "Not implemented."
  -- TODO


cfUnionBy :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfUnionBy context parameters = do
  error "Not implemented."
  -- TODO


cfIntersectBy :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfIntersectBy context parameters = do
  error "Not implemented."
  -- TODO


cfGroupBy :: CustardContext
          -> [AnyCustardValue]
          -> FruitTart AnyCustardValue
cfGroupBy context parameters = do
  error "Not implemented."
  -- TODO


cfSortBy :: CustardContext
         -> [AnyCustardValue]
         -> FruitTart AnyCustardValue
cfSortBy context parameters = do
  error "Not implemented."
  -- TODO


cfInsertBy :: CustardContext
           -> [AnyCustardValue]
           -> FruitTart AnyCustardValue
cfInsertBy context parameters = do
  error "Not implemented."
  -- TODO


cfMaximumBy :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfMaximumBy context parameters = do
  error "Not implemented."
  -- TODO


cfMinimumBy :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfMinimumBy context parameters = do
  error "Not implemented."
  -- TODO

module Network.FruitTart.Custard.Functions.Maps (
                                                 cfMapNull,
                                                 cfMapSize,
                                                 cfMapMember,
                                                 cfMapNotMember,
                                                 cfMapLookup,
                                                 cfMapFindWithDefault,
                                                 cfMakeEmptyMap,
                                                 cfMakeSingletonMap,
                                                 cfMapInsert,
                                                 cfMapInsertWith,
                                                 cfMapInsertWithKey,
                                                 cfMapInsertLookupWithKey,
                                                 cfMapDelete,
                                                 cfMapAdjust,
                                                 cfMapAdjustWithKey,
                                                 cfMapUpdate,
                                                 cfMapUpdateWithKey,
                                                 cfMapUpdateLookupWithKey,
                                                 cfMapAlter,
                                                 cfMapUnion,
                                                 cfMapUnionWith,
                                                 cfMapUnionWithKey,
                                                 cfMapUnions,
                                                 cfMapUnionsWith,
                                                 cfMapDifference,
                                                 cfMapDifferenceWith,
                                                 cfMapDifferenceWithKey,
                                                 cfMapIntersection,
                                                 cfMapIntersectionWith,
                                                 cfMapIntersectionWithKey,
                                                 cfMapMap,
                                                 cfMapMapWithKey,
                                                 cfMapMapAccum,
                                                 cfMapMapAccumWithKey,
                                                 cfMapMapAccumRWithKey,
                                                 cfMapMapKeys,
                                                 cfMapMapKeysWith,
                                                 cfMapMapKeysMonotonic,
                                                 cfMapFold,
                                                 cfMapFoldWithKey,
                                                 cfMapFoldrWithKey,
                                                 cfMapFoldlWithKey,
                                                 cfMapElems,
                                                 cfMapKeys,
                                                 cfMapKeysSet,
                                                 cfMapAssocs,
                                                 cfMapToList,
                                                 cfMapFromList,
                                                 cfMapFromListWith,
                                                 cfMapFromListWithKey,
                                                 cfMapToAscList,
                                                 cfMapToDescList,
                                                 cfMapFromAscList,
                                                 cfMapFromAscListWith,
                                                 cfMapFromAscListWithKey,
                                                 cfMapFromDistinctAscList,
                                                 cfMapFilter,
                                                 cfMapFilterWithKey,
                                                 cfMapPartition,
                                                 cfMapPartitionWithKey,
                                                 cfMapMaybe,
                                                 cfMapMaybeWithKey,
                                                 cfMapEither,
                                                 cfMapEitherWithKey,
                                                 cfMapSplit,
                                                 cfMapSplitLookup,
                                                 cfMapIsSubmapOf,
                                                 cfMapIsSubmapOfBy,
                                                 cfMapIsProperSubmapOf,
                                                 cfMapIsProperSubmapOfBy,
                                                 cfMapLookupIndex,
                                                 cfMapFindIndex,
                                                 cfMapElemAt,
                                                 cfMapUpdateAt,
                                                 cfMapDeleteAt,
                                                 cfMapFindMin,
                                                 cfMapFindMax,
                                                 cfMapDeleteMin,
                                                 cfMapDeleteMax,
                                                 cfMapDeleteFindMin,
                                                 cfMapDeleteFindMax,
                                                 cfMapUpdateMin,
                                                 cfMapUpdateMax,
                                                 cfMapUpdateMinWithKey,
                                                 cfMapUpdateMaxWithKey,
                                                 cfMapMinView,
                                                 cfMapMaxView,
                                                 cfMapMinViewWithKey,
                                                 cfMapMaxViewWithKey
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
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfMapNull :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMapNull context parameters = do
  requireNParameters parameters 1 "mapNull"
  theMap <- valueToMap $ parameters !! 0
  return $ CustardBool $ Map.null theMap


cfMapSize :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMapSize context parameters = do
  requireNParameters parameters 1 "mapSize"
  theMap <- valueToMap $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ Map.size theMap


cfMapMember :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapMember context parameters = do
  requireNParameters parameters 2 "mapMember"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return $ CustardBool $ Map.member symbol theMap


cfMapNotMember :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapNotMember context parameters = do
  requireNParameters parameters 2 "mapNotMember"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return $ CustardBool $ Map.notMember symbol theMap


cfMapLookup :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapLookup context parameters = do
  requireNParameters parameters 2 "mapLookup"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  let maybeResult = Map.lookup symbol theMap
  case maybeResult of
    Nothing -> return $ CustardMaybe Nothing
    Just result -> return $ CustardMaybe $ Just result


cfMapFindWithDefault :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfMapFindWithDefault context parameters = do
  requireNParameters parameters 3 "mapFindWithDefault"
  defaultValue <- return $ parameters !! 0
  key <- valueToSymbol $ parameters !! 1
  theMap <- valueToMap $ parameters !! 2
  return $ Map.findWithDefault defaultValue key theMap


cfMakeEmptyMap :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMakeEmptyMap context parameters = do
  requireNParameters parameters 0 "makeEmptyMap"
  return $ CustardMap $ Map.empty


cfMakeSingletonMap :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMakeSingletonMap context parameters = do
  requireNParameters parameters 2 "makeSingletonMap"
  key <- valueToSymbol $ parameters !! 0
  value <- return $ parameters !! 1
  return $ CustardMap $ Map.singleton key value


cfMapInsert :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapInsert context parameters = do
  requireNParameters parameters 3 "mapInsert"
  key <- valueToSymbol $ parameters !! 0
  value <- return $ parameters !! 1
  theMap <- valueToMap $ parameters !! 2
  return $ CustardMap $ Map.insert key value theMap


cfMapInsertWith :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapInsertWith context parameters = do
  requireNParameters parameters 0 "mapInsertWith"
  error "Not yet implemented."
  -- TODO


cfMapInsertWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapInsertWithKey context parameters = do
  requireNParameters parameters 0 "mapInsertWithKey"
  error "Not yet implemented."
  -- TODO


cfMapInsertLookupWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart CustardValue
cfMapInsertLookupWithKey context parameters = do
  requireNParameters parameters 0 "mapInsertLookupWithKey"
  error "Not yet implemented."
  -- TODO


cfMapDelete :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapDelete context parameters = do
  requireNParameters parameters 2 "mapDelete"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return $ CustardMap $ Map.delete symbol theMap


cfMapAdjust :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapAdjust context parameters = do
  requireNParameters parameters 0 "mapAdjust"
  error "Not yet implemented."
  -- TODO


cfMapAdjustWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapAdjustWithKey context parameters = do
  requireNParameters parameters 0 "mapAdjustWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdate :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapUpdate context parameters = do
  requireNParameters parameters 0 "mapUpdate"
  error "Not yet implemented."
  -- TODO


cfMapUpdateWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapUpdateWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdateLookupWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart CustardValue
cfMapUpdateLookupWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateLookupWithKey"
  error "Not yet implemented."
  -- TODO


cfMapAlter :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfMapAlter context parameters = do
  requireNParameters parameters 0 "mapAlter"
  error "Not yet implemented."
  -- TODO


cfMapUnion :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfMapUnion context parameters = do
  requireNParameters parameters 2 "mapUnion"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return $ CustardMap $ Map.union mapA mapB


cfMapUnionWith :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapUnionWith context parameters = do
  requireNParameters parameters 0 "mapUnionWith"
  error "Not yet implemented."
  -- TODO


cfMapUnionWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapUnionWithKey context parameters = do
  requireNParameters parameters 0 "mapUnionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUnions :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapUnions context parameters = do
  requireNParameters parameters 1 "mapUnions"
  maps <- valueToListOfMaps $ parameters !! 0
  return $ CustardMap $ Map.unions maps


cfMapUnionsWith :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapUnionsWith context parameters = do
  requireNParameters parameters 0 "mapUnionsWith"
  error "Not yet implemented."
  -- TODO


cfMapDifference :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapDifference context parameters = do
  requireNParameters parameters 2 "mapDifference"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return $ CustardMap $ Map.difference mapA mapB


cfMapDifferenceWith :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfMapDifferenceWith context parameters = do
  requireNParameters parameters 0 "mapDifferenceWith"
  error "Not yet implemented."
  -- TODO


cfMapDifferenceWithKey :: CustardContext
                       -> [CustardValue]
                       -> FruitTart CustardValue
cfMapDifferenceWithKey context parameters = do
  requireNParameters parameters 0 "mapDifferenceWithKey"
  error "Not yet implemented."
  -- TODO


cfMapIntersection :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapIntersection context parameters = do
  requireNParameters parameters 2 "mapIntersection"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return $ CustardMap $ Map.intersection mapA mapB


cfMapIntersectionWith :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapIntersectionWith context parameters = do
  requireNParameters parameters 0 "mapIntersectionWith"
  error "Not yet implemented."
  -- TODO


cfMapIntersectionWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart CustardValue
cfMapIntersectionWithKey context parameters = do
  requireNParameters parameters 0 "mapIntersectionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMap :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfMapMap context parameters = do
  requireNParameters parameters 0 "mapMap"
  error "Not yet implemented."
  -- TODO


cfMapMapWithKey :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapMapWithKey context parameters = do
  requireNParameters parameters 0 "mapMapWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapAccum :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfMapMapAccum context parameters = do
  requireNParameters parameters 0 "mapMapAccum"
  error "Not yet implemented."
  -- TODO


cfMapMapAccumWithKey :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfMapMapAccumWithKey context parameters = do
  requireNParameters parameters 0 "mapMapAccumWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapAccumRWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapMapAccumRWithKey context parameters = do
  requireNParameters parameters 0 "mapMapAccumRWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapKeys :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapMapKeys context parameters = do
  requireNParameters parameters 0 "mapMapKeys"
  error "Not yet implemented."
  -- TODO


cfMapMapKeysWith :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfMapMapKeysWith context parameters = do
  requireNParameters parameters 0 "mapMapKeysWith"
  error "Not yet implemented."
  -- TODO


cfMapMapKeysMonotonic :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapMapKeysMonotonic context parameters = do
  requireNParameters parameters 0 "mapMapKeysMonotonic"
  error "Not yet implemented."
  -- TODO


cfMapFold :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMapFold context parameters = do
  requireNParameters parameters 0 "mapFold"
  error "Not yet implemented."
  -- TODO


cfMapFoldWithKey :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfMapFoldWithKey context parameters = do
  requireNParameters parameters 0 "mapFoldWithKey"
  error "Not yet implemented."
  -- TODO


cfMapFoldrWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapFoldrWithKey context parameters = do
  requireNParameters parameters 0 "mapFoldrWithKey"
  error "Not yet implemented."
  -- TODO


cfMapFoldlWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapFoldlWithKey context parameters = do
  requireNParameters parameters 0 "mapFoldlWithKey"
  error "Not yet implemented."
  -- TODO


cfMapElems :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfMapElems context parameters = do
  requireNParameters parameters 1 "mapElems"
  theMap <- valueToMap $ parameters !! 0
  let items = Map.elems theMap
  typecheckList items
  return $ CustardList items


cfMapKeys :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfMapKeys context parameters = do
  requireNParameters parameters 1 "mapKeys"
  theMap <- valueToMap $ parameters !! 0
  let items = Map.keys theMap
  return $ CustardList
         $ map (\(moduleName, properName)
                  -> CustardSymbol moduleName properName)
               items


cfMapKeysSet :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapKeysSet context parameters = do
  requireNParameters parameters 0 "mapKeysSet"
  error "Not yet implemented."
  -- TODO


cfMapAssocs :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapAssocs context parameters = do
  requireNParameters parameters 1 "mapAssocs"
  theMap <- valueToMap $ parameters !! 0
  let result = map (\((moduleName, properName), value) ->
                      CustardTuple [CustardSymbol moduleName properName,
                                    value])
                   $ Map.assocs theMap
  typecheckList result
  return $ CustardList result


cfMapToList :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapToList context parameters = do
  requireNParameters parameters 1 "mapToList"
  theMap <- valueToMap $ parameters !! 0
  let result = map (\((moduleName, properName), value) ->
                      CustardTuple [CustardSymbol moduleName properName,
                                    value])
                   $ Map.assocs theMap
  typecheckList result
  return $ CustardList result


cfMapFromList :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfMapFromList context parameters = do
  requireNParameters parameters 0 "mapFromList"
  error "Not yet implemented."
  -- TODO


cfMapFromListWith :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapFromListWith context parameters = do
  requireNParameters parameters 0 "mapFromListWith"
  error "Not yet implemented."
  -- TODO


cfMapFromListWithKey :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfMapFromListWithKey context parameters = do
  requireNParameters parameters 0 "mapFromListWithKey"
  error "Not yet implemented."
  -- TODO


cfMapToAscList :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapToAscList context parameters = do
  requireNParameters parameters 0 "mapToAscList"
  error "Not yet implemented."
  -- TODO


cfMapToDescList :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapToDescList context parameters = do
  requireNParameters parameters 0 "mapToDescList"
  error "Not yet implemented."
  -- TODO


cfMapFromAscList :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfMapFromAscList context parameters = do
  requireNParameters parameters 0 "mapFromAscList"
  error "Not yet implemented."
  -- TODO


cfMapFromAscListWith :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfMapFromAscListWith context parameters = do
  requireNParameters parameters 0 "mapFromAscListWith"
  error "Not yet implemented."
  -- TODO


cfMapFromAscListWithKey :: CustardContext
                        -> [CustardValue]
                        -> FruitTart CustardValue
cfMapFromAscListWithKey context parameters = do
  requireNParameters parameters 0 "mapFromAscListWithKey"
  error "Not yet implemented."
  -- TODO


cfMapFromDistinctAscList :: CustardContext
                         -> [CustardValue]
                         -> FruitTart CustardValue
cfMapFromDistinctAscList context parameters = do
  requireNParameters parameters 0 "mapFromDistinctAscList"
  error "Not yet implemented."
  -- TODO


cfMapFilter :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapFilter context parameters = do
  requireNParameters parameters 0 "mapFilter"
  error "Not yet implemented."
  -- TODO


cfMapFilterWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapFilterWithKey context parameters = do
  requireNParameters parameters 0 "mapFilterWithKey"
  error "Not yet implemented."
  -- TODO


cfMapPartition :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapPartition context parameters = do
  requireNParameters parameters 0 "mapPartition"
  error "Not yet implemented."
  -- TODO


cfMapPartitionWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapPartitionWithKey context parameters = do
  requireNParameters parameters 0 "mapPartitionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMaybe :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfMapMaybe context parameters = do
  requireNParameters parameters 0 "mapMaybe"
  error "Not yet implemented."
  -- TODO


cfMapMaybeWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapMaybeWithKey context parameters = do
  requireNParameters parameters 0 "mapMaybeWithKey"
  error "Not yet implemented."
  -- TODO


cfMapEither :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapEither context parameters = do
  requireNParameters parameters 0 "mapEither"
  error "Not yet implemented."
  -- TODO


cfMapEitherWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapEitherWithKey context parameters = do
  requireNParameters parameters 0 "mapEitherWithKey"
  error "Not yet implemented."
  -- TODO


cfMapSplit :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfMapSplit context parameters = do
  requireNParameters parameters 0 "mapSplit"
  error "Not yet implemented."
  -- TODO


cfMapSplitLookup :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfMapSplitLookup context parameters = do
  requireNParameters parameters 0 "mapSplitLookup"
  error "Not yet implemented."
  -- TODO


cfMapIsSubmapOf :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMapIsSubmapOf context parameters = do
  requireNParameters parameters 0 "mapIsSubmapOf"
  error "Not yet implemented."
  -- TODO


cfMapIsSubmapOfBy :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfMapIsSubmapOfBy context parameters = do
  requireNParameters parameters 0 "mapIsSubmapOfBy"
  error "Not yet implemented."
  -- TODO


cfMapIsProperSubmapOf :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapIsProperSubmapOf context parameters = do
  requireNParameters parameters 0 "mapIsProperSubmapOf"
  error "Not yet implemented."
  -- TODO


cfMapIsProperSubmapOfBy :: CustardContext
                        -> [CustardValue]
                        -> FruitTart CustardValue
cfMapIsProperSubmapOfBy context parameters = do
  requireNParameters parameters 0 "mapIsProperSubmapOfBy"
  error "Not yet implemented."
  -- TODO


cfMapLookupIndex :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfMapLookupIndex context parameters = do
  requireNParameters parameters 0 "mapLookupIndex"
  error "Not yet implemented."
  -- TODO


cfMapFindIndex :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapFindIndex context parameters = do
  requireNParameters parameters 0 "mapFindIndex"
  error "Not yet implemented."
  -- TODO


cfMapElemAt :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfMapElemAt context parameters = do
  requireNParameters parameters 0 "mapElemAt"
  error "Not yet implemented."
  -- TODO


cfMapUpdateAt :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfMapUpdateAt context parameters = do
  requireNParameters parameters 0 "mapUpdateAt"
  error "Not yet implemented."
  -- TODO


cfMapDeleteAt :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfMapDeleteAt context parameters = do
  requireNParameters parameters 0 "mapDeleteAt"
  error "Not yet implemented."
  -- TODO


cfMapFindMin :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapFindMin context parameters = do
  requireNParameters parameters 0 "mapFindMin"
  error "Not yet implemented."
  -- TODO


cfMapFindMax :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapFindMax context parameters = do
  requireNParameters parameters 0 "mapFindMax"
  error "Not yet implemented."
  -- TODO


cfMapDeleteMin :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapDeleteMin context parameters = do
  requireNParameters parameters 0 "mapDeleteMin"
  error "Not yet implemented."
  -- TODO


cfMapDeleteMax :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapDeleteMax context parameters = do
  requireNParameters parameters 0 "mapDeleteMax"
  error "Not yet implemented."
  -- TODO


cfMapDeleteFindMin :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapDeleteFindMin context parameters = do
  requireNParameters parameters 0 "mapDeleteFindMin"
  error "Not yet implemented."
  -- TODO


cfMapDeleteFindMax :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfMapDeleteFindMax context parameters = do
  requireNParameters parameters 0 "mapDeleteFindMax"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMin :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapUpdateMin context parameters = do
  requireNParameters parameters 0 "mapUpdateMin"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMax :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfMapUpdateMax context parameters = do
  requireNParameters parameters 0 "mapUpdateMax"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMinWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapUpdateMinWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateMinWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMaxWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart CustardValue
cfMapUpdateMaxWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateMaxWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMinView :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapMinView context parameters = do
  requireNParameters parameters 0 "mapMinView"
  error "Not yet implemented."
  -- TODO


cfMapMaxView :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMapMaxView context parameters = do
  requireNParameters parameters 0 "mapMaxView"
  error "Not yet implemented."
  -- TODO


cfMapMinViewWithKey :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfMapMinViewWithKey context parameters = do
  requireNParameters parameters 0 "mapMinViewWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMaxViewWithKey :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfMapMaxViewWithKey context parameters = do
  requireNParameters parameters 0 "mapMaxViewWithKey"
  error "Not yet implemented."
  -- TODO
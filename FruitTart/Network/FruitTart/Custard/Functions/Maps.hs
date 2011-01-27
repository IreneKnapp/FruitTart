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


cfMapNull :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMapNull context parameters = do
  requireNParameters parameters 1 "mapNull"
  theMap <- valueToMap $ parameters !! 0
  return (context, CustardBool $ Map.null theMap)


cfMapSize :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMapSize context parameters = do
  requireNParameters parameters 1 "mapSize"
  theMap <- valueToMap $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ Map.size theMap)


cfMapMember :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapMember context parameters = do
  requireNParameters parameters 2 "mapMember"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return (context, CustardBool $ Map.member symbol theMap)


cfMapNotMember :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapNotMember context parameters = do
  requireNParameters parameters 2 "mapNotMember"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return (context, CustardBool $ Map.notMember symbol theMap)


cfMapLookup :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapLookup context parameters = do
  requireNParameters parameters 2 "mapLookup"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  let maybeResult = Map.lookup symbol theMap
  case maybeResult of
    Nothing -> return (context, CustardMaybe Nothing)
    Just result -> return (context, CustardMaybe $ Just result)


cfMapFindWithDefault :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfMapFindWithDefault context parameters = do
  requireNParameters parameters 3 "mapFindWithDefault"
  defaultValue <- return $ parameters !! 0
  key <- valueToSymbol $ parameters !! 1
  theMap <- valueToMap $ parameters !! 2
  return (context, Map.findWithDefault defaultValue key theMap)


cfMakeEmptyMap :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMakeEmptyMap context parameters = do
  requireNParameters parameters 0 "makeEmptyMap"
  return (context, CustardMap $ Map.empty)


cfMakeSingletonMap :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMakeSingletonMap context parameters = do
  requireNParameters parameters 2 "makeSingletonMap"
  key <- valueToSymbol $ parameters !! 0
  value <- return $ parameters !! 1
  return (context, CustardMap $ Map.singleton key value)


cfMapInsert :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapInsert context parameters = do
  requireNParameters parameters 3 "mapInsert"
  key <- valueToSymbol $ parameters !! 0
  value <- return $ parameters !! 1
  theMap <- valueToMap $ parameters !! 2
  return (context, CustardMap $ Map.insert key value theMap)


cfMapInsertWith :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapInsertWith context parameters = do
  requireNParameters parameters 0 "mapInsertWith"
  error "Not yet implemented."
  -- TODO


cfMapInsertWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapInsertWithKey context parameters = do
  requireNParameters parameters 0 "mapInsertWithKey"
  error "Not yet implemented."
  -- TODO


cfMapInsertLookupWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfMapInsertLookupWithKey context parameters = do
  requireNParameters parameters 0 "mapInsertLookupWithKey"
  error "Not yet implemented."
  -- TODO


cfMapDelete :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapDelete context parameters = do
  requireNParameters parameters 2 "mapDelete"
  symbol <- valueToSymbol $ parameters !! 0
  theMap <- valueToMap $ parameters !! 1
  return (context, CustardMap $ Map.delete symbol theMap)


cfMapAdjust :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapAdjust context parameters = do
  requireNParameters parameters 0 "mapAdjust"
  error "Not yet implemented."
  -- TODO


cfMapAdjustWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapAdjustWithKey context parameters = do
  requireNParameters parameters 0 "mapAdjustWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdate :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapUpdate context parameters = do
  requireNParameters parameters 0 "mapUpdate"
  error "Not yet implemented."
  -- TODO


cfMapUpdateWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapUpdateWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdateLookupWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfMapUpdateLookupWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateLookupWithKey"
  error "Not yet implemented."
  -- TODO


cfMapAlter :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapAlter context parameters = do
  requireNParameters parameters 0 "mapAlter"
  error "Not yet implemented."
  -- TODO


cfMapUnion :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapUnion context parameters = do
  requireNParameters parameters 2 "mapUnion"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return (context, CustardMap $ Map.union mapA mapB)


cfMapUnionWith :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapUnionWith context parameters = do
  requireNParameters parameters 0 "mapUnionWith"
  error "Not yet implemented."
  -- TODO


cfMapUnionWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapUnionWithKey context parameters = do
  requireNParameters parameters 0 "mapUnionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUnions :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapUnions context parameters = do
  requireNParameters parameters 1 "mapUnions"
  maps <- valueToListOfMaps $ parameters !! 0
  return (context, CustardMap $ Map.unions maps)


cfMapUnionsWith :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapUnionsWith context parameters = do
  requireNParameters parameters 0 "mapUnionsWith"
  error "Not yet implemented."
  -- TODO


cfMapDifference :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapDifference context parameters = do
  requireNParameters parameters 2 "mapDifference"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return (context, CustardMap $ Map.difference mapA mapB)


cfMapDifferenceWith :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfMapDifferenceWith context parameters = do
  requireNParameters parameters 0 "mapDifferenceWith"
  error "Not yet implemented."
  -- TODO


cfMapDifferenceWithKey :: CustardContext
                       -> [CustardValue]
                       -> FruitTart (CustardContext, CustardValue)
cfMapDifferenceWithKey context parameters = do
  requireNParameters parameters 0 "mapDifferenceWithKey"
  error "Not yet implemented."
  -- TODO


cfMapIntersection :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapIntersection context parameters = do
  requireNParameters parameters 2 "mapIntersection"
  mapA <- valueToMap $ parameters !! 0
  mapB <- valueToMap $ parameters !! 1
  return (context, CustardMap $ Map.intersection mapA mapB)


cfMapIntersectionWith :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapIntersectionWith context parameters = do
  requireNParameters parameters 0 "mapIntersectionWith"
  error "Not yet implemented."
  -- TODO


cfMapIntersectionWithKey :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfMapIntersectionWithKey context parameters = do
  requireNParameters parameters 0 "mapIntersectionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMap :: CustardContext
         -> [CustardValue]
         -> FruitTart (CustardContext, CustardValue)
cfMapMap context parameters = do
  requireNParameters parameters 0 "mapMap"
  error "Not yet implemented."
  -- TODO


cfMapMapWithKey :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapMapWithKey context parameters = do
  requireNParameters parameters 0 "mapMapWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapAccum :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMapMapAccum context parameters = do
  requireNParameters parameters 0 "mapMapAccum"
  error "Not yet implemented."
  -- TODO


cfMapMapAccumWithKey :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfMapMapAccumWithKey context parameters = do
  requireNParameters parameters 0 "mapMapAccumWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapAccumRWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapMapAccumRWithKey context parameters = do
  requireNParameters parameters 0 "mapMapAccumRWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMapKeys :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapMapKeys context parameters = do
  requireNParameters parameters 0 "mapMapKeys"
  error "Not yet implemented."
  -- TODO


cfMapMapKeysWith :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfMapMapKeysWith context parameters = do
  requireNParameters parameters 0 "mapMapKeysWith"
  error "Not yet implemented."
  -- TODO


cfMapMapKeysMonotonic :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapMapKeysMonotonic context parameters = do
  requireNParameters parameters 0 "mapMapKeysMonotonic"
  error "Not yet implemented."
  -- TODO


cfMapFold :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMapFold context parameters = do
  requireNParameters parameters 0 "mapFold"
  error "Not yet implemented."
  -- TODO


cfMapFoldrWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapFoldrWithKey context parameters = do
  requireNParameters parameters 0 "mapFoldrWithKey"
  error "Not yet implemented."
  -- TODO


cfMapFoldlWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapFoldlWithKey context parameters = do
  requireNParameters parameters 0 "mapFoldlWithKey"
  error "Not yet implemented."
  -- TODO


cfMapElems :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapElems context parameters = do
  requireNParameters parameters 1 "mapElems"
  theMap <- valueToMap $ parameters !! 0
  let items = Map.elems theMap
  typecheckList items
  return (context, CustardList items)


cfMapKeys :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfMapKeys context parameters = do
  requireNParameters parameters 1 "mapKeys"
  theMap <- valueToMap $ parameters !! 0
  let items = Map.keys theMap
  return (context,
          CustardList
           $ map (\(moduleName, properName)
                    -> CustardSymbol moduleName properName)
                 items)


cfMapKeysSet :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapKeysSet context parameters = do
  requireNParameters parameters 0 "mapKeysSet"
  error "Not yet implemented."
  -- TODO


cfMapAssocs :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapAssocs context parameters = do
  requireNParameters parameters 1 "mapAssocs"
  theMap <- valueToMap $ parameters !! 0
  let result = map (\((moduleName, properName), value) ->
                      CustardTuple [CustardSymbol moduleName properName,
                                    value])
                   $ Map.assocs theMap
  typecheckList result
  return (context, CustardList result)


cfMapToList :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapToList context parameters = do
  requireNParameters parameters 1 "mapToList"
  theMap <- valueToMap $ parameters !! 0
  let result = map (\((moduleName, properName), value) ->
                      CustardTuple [CustardSymbol moduleName properName,
                                    value])
                   $ Map.assocs theMap
  typecheckList result
  return (context, CustardList result)


cfMapFromList :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMapFromList context parameters = do
  requireNParameters parameters 0 "mapFromList"
  error "Not yet implemented."
  -- TODO


cfMapFromListWith :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapFromListWith context parameters = do
  requireNParameters parameters 0 "mapFromListWith"
  error "Not yet implemented."
  -- TODO


cfMapFromListWithKey :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfMapFromListWithKey context parameters = do
  requireNParameters parameters 0 "mapFromListWithKey"
  error "Not yet implemented."
  -- TODO


cfMapToAscList :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapToAscList context parameters = do
  requireNParameters parameters 0 "mapToAscList"
  error "Not yet implemented."
  -- TODO


cfMapToDescList :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapToDescList context parameters = do
  requireNParameters parameters 0 "mapToDescList"
  error "Not yet implemented."
  -- TODO


cfMapFromAscList :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfMapFromAscList context parameters = do
  requireNParameters parameters 0 "mapFromAscList"
  error "Not yet implemented."
  -- TODO


cfMapFromAscListWith :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfMapFromAscListWith context parameters = do
  requireNParameters parameters 0 "mapFromAscListWith"
  error "Not yet implemented."
  -- TODO


cfMapFromAscListWithKey :: CustardContext
                        -> [CustardValue]
                        -> FruitTart (CustardContext, CustardValue)
cfMapFromAscListWithKey context parameters = do
  requireNParameters parameters 0 "mapFromAscListWithKey"
  error "Not yet implemented."
  -- TODO


cfMapFromDistinctAscList :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfMapFromDistinctAscList context parameters = do
  requireNParameters parameters 0 "mapFromDistinctAscList"
  error "Not yet implemented."
  -- TODO


cfMapFilter :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapFilter context parameters = do
  requireNParameters parameters 0 "mapFilter"
  error "Not yet implemented."
  -- TODO


cfMapFilterWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapFilterWithKey context parameters = do
  requireNParameters parameters 0 "mapFilterWithKey"
  error "Not yet implemented."
  -- TODO


cfMapPartition :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapPartition context parameters = do
  requireNParameters parameters 0 "mapPartition"
  error "Not yet implemented."
  -- TODO


cfMapPartitionWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapPartitionWithKey context parameters = do
  requireNParameters parameters 0 "mapPartitionWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMaybe :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapMaybe context parameters = do
  requireNParameters parameters 0 "mapMaybe"
  error "Not yet implemented."
  -- TODO


cfMapMaybeWithKey :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapMaybeWithKey context parameters = do
  requireNParameters parameters 0 "mapMaybeWithKey"
  error "Not yet implemented."
  -- TODO


cfMapEither :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapEither context parameters = do
  requireNParameters parameters 0 "mapEither"
  error "Not yet implemented."
  -- TODO


cfMapEitherWithKey :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapEitherWithKey context parameters = do
  requireNParameters parameters 0 "mapEitherWithKey"
  error "Not yet implemented."
  -- TODO


cfMapSplit :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfMapSplit context parameters = do
  requireNParameters parameters 0 "mapSplit"
  error "Not yet implemented."
  -- TODO


cfMapSplitLookup :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfMapSplitLookup context parameters = do
  requireNParameters parameters 0 "mapSplitLookup"
  error "Not yet implemented."
  -- TODO


cfMapIsSubmapOf :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfMapIsSubmapOf context parameters = do
  requireNParameters parameters 0 "mapIsSubmapOf"
  error "Not yet implemented."
  -- TODO


cfMapIsSubmapOfBy :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfMapIsSubmapOfBy context parameters = do
  requireNParameters parameters 0 "mapIsSubmapOfBy"
  error "Not yet implemented."
  -- TODO


cfMapIsProperSubmapOf :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapIsProperSubmapOf context parameters = do
  requireNParameters parameters 0 "mapIsProperSubmapOf"
  error "Not yet implemented."
  -- TODO


cfMapIsProperSubmapOfBy :: CustardContext
                        -> [CustardValue]
                        -> FruitTart (CustardContext, CustardValue)
cfMapIsProperSubmapOfBy context parameters = do
  requireNParameters parameters 0 "mapIsProperSubmapOfBy"
  error "Not yet implemented."
  -- TODO


cfMapLookupIndex :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfMapLookupIndex context parameters = do
  requireNParameters parameters 0 "mapLookupIndex"
  error "Not yet implemented."
  -- TODO


cfMapFindIndex :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapFindIndex context parameters = do
  requireNParameters parameters 0 "mapFindIndex"
  error "Not yet implemented."
  -- TODO


cfMapElemAt :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfMapElemAt context parameters = do
  requireNParameters parameters 0 "mapElemAt"
  error "Not yet implemented."
  -- TODO


cfMapUpdateAt :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMapUpdateAt context parameters = do
  requireNParameters parameters 0 "mapUpdateAt"
  error "Not yet implemented."
  -- TODO


cfMapDeleteAt :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfMapDeleteAt context parameters = do
  requireNParameters parameters 0 "mapDeleteAt"
  error "Not yet implemented."
  -- TODO


cfMapFindMin :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapFindMin context parameters = do
  requireNParameters parameters 0 "mapFindMin"
  error "Not yet implemented."
  -- TODO


cfMapFindMax :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapFindMax context parameters = do
  requireNParameters parameters 0 "mapFindMax"
  error "Not yet implemented."
  -- TODO


cfMapDeleteMin :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapDeleteMin context parameters = do
  requireNParameters parameters 0 "mapDeleteMin"
  error "Not yet implemented."
  -- TODO


cfMapDeleteMax :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapDeleteMax context parameters = do
  requireNParameters parameters 0 "mapDeleteMax"
  error "Not yet implemented."
  -- TODO


cfMapDeleteFindMin :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapDeleteFindMin context parameters = do
  requireNParameters parameters 0 "mapDeleteFindMin"
  error "Not yet implemented."
  -- TODO


cfMapDeleteFindMax :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfMapDeleteFindMax context parameters = do
  requireNParameters parameters 0 "mapDeleteFindMax"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMin :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapUpdateMin context parameters = do
  requireNParameters parameters 0 "mapUpdateMin"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMax :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfMapUpdateMax context parameters = do
  requireNParameters parameters 0 "mapUpdateMax"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMinWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapUpdateMinWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateMinWithKey"
  error "Not yet implemented."
  -- TODO


cfMapUpdateMaxWithKey :: CustardContext
                      -> [CustardValue]
                      -> FruitTart (CustardContext, CustardValue)
cfMapUpdateMaxWithKey context parameters = do
  requireNParameters parameters 0 "mapUpdateMaxWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMinView :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapMinView context parameters = do
  requireNParameters parameters 0 "mapMinView"
  error "Not yet implemented."
  -- TODO


cfMapMaxView :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMapMaxView context parameters = do
  requireNParameters parameters 0 "mapMaxView"
  error "Not yet implemented."
  -- TODO


cfMapMinViewWithKey :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfMapMinViewWithKey context parameters = do
  requireNParameters parameters 0 "mapMinViewWithKey"
  error "Not yet implemented."
  -- TODO


cfMapMaxViewWithKey :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfMapMaxViewWithKey context parameters = do
  requireNParameters parameters 0 "mapMaxViewWithKey"
  error "Not yet implemented."
  -- TODO

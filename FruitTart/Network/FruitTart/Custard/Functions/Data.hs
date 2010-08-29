module Network.FruitTart.Custard.Functions.Data (
                                                 cfMakeEmptyData,
                                                 cfMakeSingletonData,
                                                 cfDataPack,
                                                 cfDataUnpack,
                                                 cfDataCons,
                                                 cfDataSnoc,
                                                 cfDataAppend,
                                                 cfDataHead,
                                                 cfDataUncons,
                                                 cfDataLast,
                                                 cfDataTail,
                                                 cfDataInit,
                                                 cfDataNull,
                                                 cfDataLength,
                                                 cfDataMap,
                                                 cfDataReverse,
                                                 cfDataIntersperse,
                                                 cfDataIntercalate,
                                                 cfDataTranspose,
                                                 cfDataFoldl,
                                                 cfDataFoldl1,
                                                 cfDataFoldr,
                                                 cfDataFoldr1,
                                                 cfDataConcat,
                                                 cfDataConcatMap,
                                                 cfDataAny,
                                                 cfDataAll,
                                                 cfDataMaximum,
                                                 cfDataMinimum,
                                                 cfDataScanl,
                                                 cfDataScanl1,
                                                 cfDataScanr,
                                                 cfDataScanr1,
                                                 cfDataMapAccumL,
                                                 cfDataMapAccumR,
                                                 cfDataReplicate,
                                                 cfDataUnfoldr,
                                                 cfDataUnfoldrN,
                                                 cfDataTake,
                                                 cfDataDrop,
                                                 cfDataSplitAt,
                                                 cfDataTakeWhile,
                                                 cfDataDropWhile,
                                                 cfDataSpan,
                                                 cfDataSpanEnd,
                                                 cfDataBreak,
                                                 cfDataBreakEnd,
                                                 cfDataBreakByte,
                                                 cfDataGroup,
                                                 cfDataGroupBy,
                                                 cfDataInits,
                                                 cfDataTails,
                                                 cfDataSplit,
                                                 cfDataSplitWith,
                                                 cfDataIsPrefixOf,
                                                 cfDataIsSuffixOf,
                                                 cfDataIsInfixOf,
                                                 cfDataBreakSubstring,
                                                 cfDataFindSubstring,
                                                 cfDataFindSubstrings,
                                                 cfDataElem,
                                                 cfDataNotElem,
                                                 cfDataFind,
                                                 cfDataFilter,
                                                 cfDataPartition,
                                                 cfDataIndex,
                                                 cfDataElemIndex,
                                                 cfDataElemIndices,
                                                 cfDataElemIndexEnd,
                                                 cfDataFindIndex,
                                                 cfDataFindIndices,
                                                 cfDataCount,
                                                 cfDataZip,
                                                 cfDataZipWith,
                                                 cfDataUnzip,
                                                 cfDataSort,
                                                 cfDataCopy,
                                                 cfDataUTF8Decode,
                                                 cfDataUTF8Uncons,
                                                 cfDataUTF8SplitAt,
                                                 cfDataUTF8Take,
                                                 cfDataUTF8Drop,
                                                 cfDataUTF8Span,
                                                 cfDataUTF8Break,
                                                 cfDataUTF8FromString,
                                                 cfDataUTF8ToString,
                                                 cfDataUTF8Foldl,
                                                 cfDataUTF8Foldr,
                                                 cfDataUTF8Length,
                                                 cfDataUTF8Lines
                                                )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import Data.Int
import Data.List
import Data.Maybe
import Prelude hiding (catch)

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Types
import Network.FruitTart.Util


cfMakeEmptyData :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfMakeEmptyData context parameters = do
  requireNParameters parameters 0 "makeEmptyData"
  return $ CustardData $ BS.empty


cfMakeSingletonData :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfMakeSingletonData context parameters = do
  requireNParameters parameters 1 "makeSingletonData"
  word8 <- valueToWord8 $ parameters !! 0
  return $ CustardData $ BS.singleton word8


cfDataPack :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataPack context parameters = do
  requireNParameters parameters 1 "dataPack"
  word8s <- valueToListOfWord8s $ parameters !! 0
  return $ CustardData $ BS.pack word8s


cfDataUnpack :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataUnpack context parameters = do
  requireNParameters parameters 1 "dataUnpack"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardList
         $ map (CustardInteger . fromIntegral)
         $ BS.unpack bytestring


cfDataCons :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataCons context parameters = do
  requireNParameters parameters 2 "dataCons"
  word8 <- valueToWord8 $ parameters !! 0
  bytestring <- valueToByteString $ parameters !! 1
  return $ CustardData $ BS.cons word8 bytestring


cfDataSnoc :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataSnoc context parameters = do
  requireNParameters parameters 2 "dataSnoc"
  bytestring <- valueToByteString $ parameters !! 0
  word8 <- valueToWord8 $ parameters !! 1
  return $ CustardData $ BS.snoc bytestring word8


cfDataAppend :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataAppend context parameters = do
  requireNParameters parameters 2 "dataAppend"
  a <- valueToByteString $ parameters !! 0
  b <- valueToByteString $ parameters !! 1
  return $ CustardData $ BS.append a b


cfDataHead :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataHead context parameters = do
  requireNParameters parameters 1 "dataHead"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ BS.head bytestring


cfDataUncons :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataUncons context parameters = do
  requireNParameters parameters 1 "dataUncons"
  bytestring <- valueToByteString $ parameters !! 0
  return $ case BS.uncons bytestring of
             Nothing -> CustardMaybe $ Nothing
             Just (word8, rest) -> CustardMaybe
                                   $ Just
                                   $ CustardTuple [CustardInteger
                                                   $ fromIntegral word8,
                                                   CustardData rest]


cfDataLast :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataLast context parameters = do
  requireNParameters parameters 1 "dataLast"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ BS.last bytestring


cfDataTail :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataTail context parameters = do
  requireNParameters parameters 1 "dataTail"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardData $ BS.tail bytestring


cfDataInit :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataInit context parameters = do
  requireNParameters parameters 1 "dataInit"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardData $ BS.init bytestring


cfDataNull :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataNull context parameters = do
  requireNParameters parameters 1 "dataNull"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardBool $ BS.null bytestring


cfDataLength :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataLength context parameters = do
  requireNParameters parameters 1 "dataLength"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ BS.length bytestring


cfDataMap :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfDataMap context parameters = do
  requireNParameters parameters 0 "dataMap"
  error "Not yet implemented."
  -- TODO


cfDataReverse :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataReverse context parameters = do
  requireNParameters parameters 1 "dataReverse"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardData $ BS.reverse bytestring


cfDataIntersperse :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfDataIntersperse context parameters = do
  requireNParameters parameters 2 "dataIntersperse"
  word8 <- valueToWord8 $ parameters !! 0
  bytestring <- valueToByteString $ parameters !! 1
  return $ CustardData $ BS.intersperse word8 bytestring


cfDataIntercalate :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfDataIntercalate context parameters = do
  requireNParameters parameters 2 "dataIntercalate"
  a <- valueToByteString $ parameters !! 0
  bs <- valueToListOfByteStrings $ parameters !! 1
  return $ CustardData $ BS.intercalate a bs


cfDataTranspose :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataTranspose context parameters = do
  requireNParameters parameters 1 "dataTranspose"
  bytestrings <- valueToListOfByteStrings $ parameters !! 0
  return $ CustardList $ map CustardData $ BS.transpose bytestrings


cfDataFoldl :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataFoldl context parameters = do
  requireNParameters parameters 0 "dataFoldl"
  error "Not yet implemented."
  -- TODO


cfDataFoldl1 :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataFoldl1 context parameters = do
  requireNParameters parameters 0 "dataFoldl1"
  error "Not yet implemented."
  -- TODO


cfDataFoldr :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataFoldr context parameters = do
  requireNParameters parameters 0 "dataFoldr"
  error "Not yet implemented."
  -- TODO


cfDataFoldr1 :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataFoldr1 context parameters = do
  requireNParameters parameters 0 "dataFoldr1"
  error "Not yet implemented."
  -- TODO


cfDataConcat :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataConcat context parameters = do
  requireNParameters parameters 1 "dataConcat"
  bytestrings <- valueToListOfByteStrings $ parameters !! 0
  return $ CustardData $ BS.concat bytestrings


cfDataConcatMap :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataConcatMap context parameters = do
  requireNParameters parameters 0 "dataConcatMap"
  error "Not yet implemented."
  -- TODO


cfDataAny :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfDataAny context parameters = do
  requireNParameters parameters 0 "dataAny"
  error "Not yet implemented."
  -- TODO


cfDataAll :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfDataAll context parameters = do
  requireNParameters parameters 0 "dataAll"
  error "Not yet implemented."
  -- TODO


cfDataMaximum :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataMaximum context parameters = do
  requireNParameters parameters 1 "dataMaximum"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ BS.maximum bytestring


cfDataMinimum :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataMinimum context parameters = do
  requireNParameters parameters 1 "dataMinimum"
  bytestring <- valueToByteString $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ BS.minimum bytestring


cfDataScanl :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataScanl context parameters = do
  requireNParameters parameters 0 "dataScanl"
  error "Not yet implemented."
  -- TODO


cfDataScanl1 :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataScanl1 context parameters = do
  requireNParameters parameters 0 "dataScanl1"
  error "Not yet implemented."
  -- TODO


cfDataScanr :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataScanr context parameters = do
  requireNParameters parameters 0 "dataScanr"
  error "Not yet implemented."
  -- TODO


cfDataScanr1 :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataScanr1 context parameters = do
  requireNParameters parameters 0 "dataScanr1"
  error "Not yet implemented."
  -- TODO


cfDataMapAccumL :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataMapAccumL context parameters = do
  requireNParameters parameters 0 "dataMapAccumL"
  error "Not yet implemented."
  -- TODO


cfDataMapAccumR :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataMapAccumR context parameters = do
  requireNParameters parameters 0 "dataMapAccumR"
  error "Not yet implemented."
  -- TODO


cfDataReplicate :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataReplicate context parameters = do
  requireNParameters parameters 0 "dataReplicate"
  error "Not yet implemented."
  -- TODO


cfDataUnfoldr :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataUnfoldr context parameters = do
  requireNParameters parameters 0 "dataUnfoldr"
  error "Not yet implemented."
  -- TODO


cfDataUnfoldrN :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDataUnfoldrN context parameters = do
  requireNParameters parameters 0 "dataUnfoldrN"
  error "Not yet implemented."
  -- TODO


cfDataTake :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataTake context parameters = do
  requireNParameters parameters 0 "dataTake"
  error "Not yet implemented."
  -- TODO


cfDataDrop :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataDrop context parameters = do
  requireNParameters parameters 0 "dataDrop"
  error "Not yet implemented."
  -- TODO


cfDataSplitAt :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataSplitAt context parameters = do
  requireNParameters parameters 0 "dataSplitAt"
  error "Not yet implemented."
  -- TODO


cfDataTakeWhile :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataTakeWhile context parameters = do
  requireNParameters parameters 0 "dataTakeWhile"
  error "Not yet implemented."
  -- TODO


cfDataDropWhile :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataDropWhile context parameters = do
  requireNParameters parameters 0 "dataDropWhile"
  error "Not yet implemented."
  -- TODO


cfDataSpan :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataSpan context parameters = do
  requireNParameters parameters 0 "dataSpan"
  error "Not yet implemented."
  -- TODO


cfDataSpanEnd :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataSpanEnd context parameters = do
  requireNParameters parameters 0 "dataSpanEnd"
  error "Not yet implemented."
  -- TODO


cfDataBreak :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataBreak context parameters = do
  requireNParameters parameters 0 "dataBreak"
  error "Not yet implemented."
  -- TODO


cfDataBreakEnd :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDataBreakEnd context parameters = do
  requireNParameters parameters 0 "dataBreakEnd"
  error "Not yet implemented."
  -- TODO


cfDataBreakByte :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataBreakByte context parameters = do
  requireNParameters parameters 0 "dataBreakByte"
  error "Not yet implemented."
  -- TODO


cfDataGroup :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataGroup context parameters = do
  requireNParameters parameters 0 "dataGroup"
  error "Not yet implemented."
  -- TODO


cfDataGroupBy :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataGroupBy context parameters = do
  requireNParameters parameters 0 "dataGroupBy"
  error "Not yet implemented."
  -- TODO


cfDataInits :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataInits context parameters = do
  requireNParameters parameters 0 "dataInits"
  error "Not yet implemented."
  -- TODO


cfDataTails :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataTails context parameters = do
  requireNParameters parameters 0 "dataTails"
  error "Not yet implemented."
  -- TODO


cfDataSplit :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataSplit context parameters = do
  requireNParameters parameters 0 "dataSplit"
  error "Not yet implemented."
  -- TODO


cfDataSplitWith :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataSplitWith context parameters = do
  requireNParameters parameters 0 "dataSplitWith"
  error "Not yet implemented."
  -- TODO


cfDataIsPrefixOf :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDataIsPrefixOf context parameters = do
  requireNParameters parameters 0 "dataIsPrefixOf"
  error "Not yet implemented."
  -- TODO


cfDataIsSuffixOf :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDataIsSuffixOf context parameters = do
  requireNParameters parameters 0 "dataIsSuffixOf"
  error "Not yet implemented."
  -- TODO


cfDataIsInfixOf :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataIsInfixOf context parameters = do
  requireNParameters parameters 0 "dataIsInfixOf"
  error "Not yet implemented."
  -- TODO


cfDataBreakSubstring :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfDataBreakSubstring context parameters = do
  requireNParameters parameters 0 "dataBreakSubstring"
  error "Not yet implemented."
  -- TODO


cfDataFindSubstring :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfDataFindSubstring context parameters = do
  requireNParameters parameters 0 "dataFindSubstring"
  error "Not yet implemented."
  -- TODO


cfDataFindSubstrings :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfDataFindSubstrings context parameters = do
  requireNParameters parameters 0 "dataFindSubstrings"
  error "Not yet implemented."
  -- TODO


cfDataElem :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataElem context parameters = do
  requireNParameters parameters 0 "dataElem"
  error "Not yet implemented."
  -- TODO


cfDataNotElem :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataNotElem context parameters = do
  requireNParameters parameters 0 "dataNotElem"
  error "Not yet implemented."
  -- TODO


cfDataFind :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataFind context parameters = do
  requireNParameters parameters 0 "dataFind"
  error "Not yet implemented."
  -- TODO


cfDataFilter :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfDataFilter context parameters = do
  requireNParameters parameters 0 "dataFilter"
  error "Not yet implemented."
  -- TODO


cfDataPartition :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataPartition context parameters = do
  requireNParameters parameters 0 "dataPartition"
  error "Not yet implemented."
  -- TODO


cfDataIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataIndex context parameters = do
  requireNParameters parameters 0 "dataIndex"
  error "Not yet implemented."
  -- TODO


cfDataElemIndex :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataElemIndex context parameters = do
  requireNParameters parameters 0 "dataElemIndex"
  error "Not yet implemented."
  -- TODO


cfDataElemIndices :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfDataElemIndices context parameters = do
  requireNParameters parameters 0 "dataElemIndices"
  error "Not yet implemented."
  -- TODO


cfDataElemIndexEnd :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfDataElemIndexEnd context parameters = do
  requireNParameters parameters 0 "dataElemIndexEnd"
  error "Not yet implemented."
  -- TODO


cfDataFindIndex :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataFindIndex context parameters = do
  requireNParameters parameters 0 "dataFindIndex"
  error "Not yet implemented."
  -- TODO


cfDataFindIndices :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfDataFindIndices context parameters = do
  requireNParameters parameters 0 "dataFindIndices"
  error "Not yet implemented."
  -- TODO


cfDataCount :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataCount context parameters = do
  requireNParameters parameters 0 "dataCount"
  error "Not yet implemented."
  -- TODO


cfDataZip :: CustardContext
          -> [CustardValue]
          -> FruitTart CustardValue
cfDataZip context parameters = do
  requireNParameters parameters 0 "dataZip"
  error "Not yet implemented."
  -- TODO


cfDataZipWith :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfDataZipWith context parameters = do
  requireNParameters parameters 0 "dataZipWith"
  error "Not yet implemented."
  -- TODO


cfDataUnzip :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfDataUnzip context parameters = do
  requireNParameters parameters 0 "dataUnzip"
  error "Not yet implemented."
  -- TODO


cfDataSort :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataSort context parameters = do
  requireNParameters parameters 0 "dataSort"
  error "Not yet implemented."
  -- TODO


cfDataCopy :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfDataCopy context parameters = do
  requireNParameters parameters 0 "dataCopy"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Decode :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDataUTF8Decode context parameters = do
  requireNParameters parameters 0 "dataUTF8Decode"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Uncons :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDataUTF8Uncons context parameters = do
  requireNParameters parameters 0 "dataUTF8Uncons"
  error "Not yet implemented."
  -- TODO


cfDataUTF8SplitAt :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfDataUTF8SplitAt context parameters = do
  requireNParameters parameters 0 "dataUTF8SplitAt"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Take :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDataUTF8Take context parameters = do
  requireNParameters parameters 0 "dataUTF8Take"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Drop :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDataUTF8Drop context parameters = do
  requireNParameters parameters 0 "dataUTF8Drop"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Span :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfDataUTF8Span context parameters = do
  requireNParameters parameters 0 "dataUTF8Span"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Break :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataUTF8Break context parameters = do
  requireNParameters parameters 0 "dataUTF8Break"
  error "Not yet implemented."
  -- TODO


cfDataUTF8FromString :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfDataUTF8FromString context parameters = do
  requireNParameters parameters 0 "dataUTF8FromString"
  error "Not yet implemented."
  -- TODO


cfDataUTF8ToString :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfDataUTF8ToString context parameters = do
  requireNParameters parameters 0 "dataUTF8ToString"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Foldl :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataUTF8Foldl context parameters = do
  requireNParameters parameters 0 "dataUTF8Foldl"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Foldr :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataUTF8Foldr context parameters = do
  requireNParameters parameters 0 "dataUTF8Foldr"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Length :: CustardContext
                 -> [CustardValue]
                 -> FruitTart CustardValue
cfDataUTF8Length context parameters = do
  requireNParameters parameters 0 "dataUTF8Length"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Lines :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfDataUTF8Lines context parameters = do
  requireNParameters parameters 0 "dataUTF8Lines"
  error "Not yet implemented."
  -- TODO

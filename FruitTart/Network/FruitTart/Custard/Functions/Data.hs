module Network.FruitTart.Custard.Functions.Data (
                                                 cfMakeEmptyData,
                                                 cfMakeSingletonData,
                                                 cfMakeUTF8DataFromString,
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
                                                 cfDataFold,
                                                 cfDataFold1,
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
                                                 cfDataUTF8Fold,
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
                -> FruitTart (CustardContext, CustardValue)
cfMakeEmptyData context parameters = do
  requireNParameters parameters 0 "makeEmptyData"
  return (context, CustardData $ BS.empty)


cfMakeSingletonData :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfMakeSingletonData context parameters = do
  requireNParameters parameters 1 "makeSingletonData"
  word8 <- valueToWord8 $ parameters !! 0
  return (context, CustardData $ BS.singleton word8)


cfMakeUTF8DataFromString :: CustardContext
                         -> [CustardValue]
                         -> FruitTart (CustardContext, CustardValue)
cfMakeUTF8DataFromString context parameters = do
  requireNParameters parameters 1 "makeUTF8DataFromString"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context, CustardData bytestring)


cfDataPack :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataPack context parameters = do
  requireNParameters parameters 1 "dataPack"
  word8s <- valueToListOfWord8s $ parameters !! 0
  return (context, CustardData $ BS.pack word8s)


cfDataUnpack :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataUnpack context parameters = do
  requireNParameters parameters 1 "dataUnpack"
  bytestring <- valueToByteString $ parameters !! 0
  return (context,
          CustardList
           $ map (CustardInteger . fromIntegral)
                 $ BS.unpack bytestring)


cfDataCons :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataCons context parameters = do
  requireNParameters parameters 2 "dataCons"
  word8 <- valueToWord8 $ parameters !! 0
  bytestring <- valueToByteString $ parameters !! 1
  return (context, CustardData $ BS.cons word8 bytestring)


cfDataSnoc :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataSnoc context parameters = do
  requireNParameters parameters 2 "dataSnoc"
  bytestring <- valueToByteString $ parameters !! 0
  word8 <- valueToWord8 $ parameters !! 1
  return (context, CustardData $ BS.snoc bytestring word8)


cfDataAppend :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataAppend context parameters = do
  requireNParameters parameters 2 "dataAppend"
  a <- valueToByteString $ parameters !! 0
  b <- valueToByteString $ parameters !! 1
  return (context, CustardData $ BS.append a b)


cfDataHead :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataHead context parameters = do
  requireNParameters parameters 1 "dataHead"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ BS.head bytestring)


cfDataUncons :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataUncons context parameters = do
  requireNParameters parameters 1 "dataUncons"
  bytestring <- valueToByteString $ parameters !! 0
  return (context,
          case BS.uncons bytestring of
            Nothing -> CustardMaybe $ Nothing
            Just (word8, rest) -> CustardMaybe
                                   $ Just
                                   $ CustardTuple [CustardInteger
                                                    $ fromIntegral word8,
                                                   CustardData rest])


cfDataLast :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataLast context parameters = do
  requireNParameters parameters 1 "dataLast"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ BS.last bytestring)


cfDataTail :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataTail context parameters = do
  requireNParameters parameters 1 "dataTail"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardData $ BS.tail bytestring)


cfDataInit :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataInit context parameters = do
  requireNParameters parameters 1 "dataInit"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardData $ BS.init bytestring)


cfDataNull :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataNull context parameters = do
  requireNParameters parameters 1 "dataNull"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardBool $ BS.null bytestring)


cfDataLength :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataLength context parameters = do
  requireNParameters parameters 1 "dataLength"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ BS.length bytestring)


cfDataMap :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfDataMap context parameters = do
  requireNParameters parameters 0 "dataMap"
  error "Not yet implemented."
  -- TODO


cfDataReverse :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataReverse context parameters = do
  requireNParameters parameters 1 "dataReverse"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardData $ BS.reverse bytestring)


cfDataIntersperse :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfDataIntersperse context parameters = do
  requireNParameters parameters 2 "dataIntersperse"
  word8 <- valueToWord8 $ parameters !! 0
  bytestring <- valueToByteString $ parameters !! 1
  return (context, CustardData $ BS.intersperse word8 bytestring)


cfDataIntercalate :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfDataIntercalate context parameters = do
  requireNParameters parameters 2 "dataIntercalate"
  a <- valueToByteString $ parameters !! 0
  bs <- valueToListOfByteStrings $ parameters !! 1
  return (context, CustardData $ BS.intercalate a bs)


cfDataTranspose :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataTranspose context parameters = do
  requireNParameters parameters 1 "dataTranspose"
  bytestrings <- valueToListOfByteStrings $ parameters !! 0
  return (context, CustardList $ map CustardData $ BS.transpose bytestrings)


cfDataFold :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataFold context parameters = do
  requireNParameters parameters 0 "dataFold"
  error "Not yet implemented."
  -- TODO


cfDataFold1 :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataFold1 context parameters = do
  requireNParameters parameters 0 "dataFold1"
  error "Not yet implemented."
  -- TODO


cfDataConcat :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataConcat context parameters = do
  requireNParameters parameters 1 "dataConcat"
  bytestrings <- valueToListOfByteStrings $ parameters !! 0
  return (context, CustardData $ BS.concat bytestrings)


cfDataConcatMap :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataConcatMap context parameters = do
  requireNParameters parameters 0 "dataConcatMap"
  error "Not yet implemented."
  -- TODO


cfDataAny :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfDataAny context parameters = do
  requireNParameters parameters 0 "dataAny"
  error "Not yet implemented."
  -- TODO


cfDataAll :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfDataAll context parameters = do
  requireNParameters parameters 0 "dataAll"
  error "Not yet implemented."
  -- TODO


cfDataMaximum :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataMaximum context parameters = do
  requireNParameters parameters 1 "dataMaximum"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ BS.maximum bytestring)


cfDataMinimum :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataMinimum context parameters = do
  requireNParameters parameters 1 "dataMinimum"
  bytestring <- valueToByteString $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ BS.minimum bytestring)


cfDataScanl :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataScanl context parameters = do
  requireNParameters parameters 0 "dataScanl"
  error "Not yet implemented."
  -- TODO


cfDataScanl1 :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataScanl1 context parameters = do
  requireNParameters parameters 0 "dataScanl1"
  error "Not yet implemented."
  -- TODO


cfDataScanr :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataScanr context parameters = do
  requireNParameters parameters 0 "dataScanr"
  error "Not yet implemented."
  -- TODO


cfDataScanr1 :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataScanr1 context parameters = do
  requireNParameters parameters 0 "dataScanr1"
  error "Not yet implemented."
  -- TODO


cfDataMapAccumL :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataMapAccumL context parameters = do
  requireNParameters parameters 0 "dataMapAccumL"
  error "Not yet implemented."
  -- TODO


cfDataMapAccumR :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataMapAccumR context parameters = do
  requireNParameters parameters 0 "dataMapAccumR"
  error "Not yet implemented."
  -- TODO


cfDataReplicate :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataReplicate context parameters = do
  requireNParameters parameters 0 "dataReplicate"
  error "Not yet implemented."
  -- TODO


cfDataUnfoldr :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataUnfoldr context parameters = do
  requireNParameters parameters 0 "dataUnfoldr"
  error "Not yet implemented."
  -- TODO


cfDataUnfoldrN :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataUnfoldrN context parameters = do
  requireNParameters parameters 0 "dataUnfoldrN"
  error "Not yet implemented."
  -- TODO


cfDataTake :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataTake context parameters = do
  requireNParameters parameters 0 "dataTake"
  error "Not yet implemented."
  -- TODO


cfDataDrop :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataDrop context parameters = do
  requireNParameters parameters 0 "dataDrop"
  error "Not yet implemented."
  -- TODO


cfDataSplitAt :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataSplitAt context parameters = do
  requireNParameters parameters 0 "dataSplitAt"
  error "Not yet implemented."
  -- TODO


cfDataTakeWhile :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataTakeWhile context parameters = do
  requireNParameters parameters 0 "dataTakeWhile"
  error "Not yet implemented."
  -- TODO


cfDataDropWhile :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataDropWhile context parameters = do
  requireNParameters parameters 0 "dataDropWhile"
  error "Not yet implemented."
  -- TODO


cfDataSpan :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataSpan context parameters = do
  requireNParameters parameters 0 "dataSpan"
  error "Not yet implemented."
  -- TODO


cfDataSpanEnd :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataSpanEnd context parameters = do
  requireNParameters parameters 0 "dataSpanEnd"
  error "Not yet implemented."
  -- TODO


cfDataBreak :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataBreak context parameters = do
  requireNParameters parameters 0 "dataBreak"
  error "Not yet implemented."
  -- TODO


cfDataBreakEnd :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataBreakEnd context parameters = do
  requireNParameters parameters 0 "dataBreakEnd"
  error "Not yet implemented."
  -- TODO


cfDataBreakByte :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataBreakByte context parameters = do
  requireNParameters parameters 0 "dataBreakByte"
  error "Not yet implemented."
  -- TODO


cfDataGroup :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataGroup context parameters = do
  requireNParameters parameters 0 "dataGroup"
  error "Not yet implemented."
  -- TODO


cfDataGroupBy :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataGroupBy context parameters = do
  requireNParameters parameters 0 "dataGroupBy"
  error "Not yet implemented."
  -- TODO


cfDataInits :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataInits context parameters = do
  requireNParameters parameters 0 "dataInits"
  error "Not yet implemented."
  -- TODO


cfDataTails :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataTails context parameters = do
  requireNParameters parameters 0 "dataTails"
  error "Not yet implemented."
  -- TODO


cfDataSplit :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataSplit context parameters = do
  requireNParameters parameters 0 "dataSplit"
  error "Not yet implemented."
  -- TODO


cfDataSplitWith :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataSplitWith context parameters = do
  requireNParameters parameters 0 "dataSplitWith"
  error "Not yet implemented."
  -- TODO


cfDataIsPrefixOf :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDataIsPrefixOf context parameters = do
  requireNParameters parameters 0 "dataIsPrefixOf"
  error "Not yet implemented."
  -- TODO


cfDataIsSuffixOf :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDataIsSuffixOf context parameters = do
  requireNParameters parameters 0 "dataIsSuffixOf"
  error "Not yet implemented."
  -- TODO


cfDataIsInfixOf :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataIsInfixOf context parameters = do
  requireNParameters parameters 0 "dataIsInfixOf"
  error "Not yet implemented."
  -- TODO


cfDataBreakSubstring :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfDataBreakSubstring context parameters = do
  requireNParameters parameters 0 "dataBreakSubstring"
  error "Not yet implemented."
  -- TODO


cfDataFindSubstring :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfDataFindSubstring context parameters = do
  requireNParameters parameters 0 "dataFindSubstring"
  error "Not yet implemented."
  -- TODO


cfDataFindSubstrings :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfDataFindSubstrings context parameters = do
  requireNParameters parameters 0 "dataFindSubstrings"
  error "Not yet implemented."
  -- TODO


cfDataElem :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataElem context parameters = do
  requireNParameters parameters 0 "dataElem"
  error "Not yet implemented."
  -- TODO


cfDataNotElem :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataNotElem context parameters = do
  requireNParameters parameters 0 "dataNotElem"
  error "Not yet implemented."
  -- TODO


cfDataFind :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataFind context parameters = do
  requireNParameters parameters 0 "dataFind"
  error "Not yet implemented."
  -- TODO


cfDataFilter :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfDataFilter context parameters = do
  requireNParameters parameters 0 "dataFilter"
  error "Not yet implemented."
  -- TODO


cfDataPartition :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataPartition context parameters = do
  requireNParameters parameters 0 "dataPartition"
  error "Not yet implemented."
  -- TODO


cfDataIndex :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataIndex context parameters = do
  requireNParameters parameters 0 "dataIndex"
  error "Not yet implemented."
  -- TODO


cfDataElemIndex :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataElemIndex context parameters = do
  requireNParameters parameters 0 "dataElemIndex"
  error "Not yet implemented."
  -- TODO


cfDataElemIndices :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfDataElemIndices context parameters = do
  requireNParameters parameters 0 "dataElemIndices"
  error "Not yet implemented."
  -- TODO


cfDataElemIndexEnd :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfDataElemIndexEnd context parameters = do
  requireNParameters parameters 0 "dataElemIndexEnd"
  error "Not yet implemented."
  -- TODO


cfDataFindIndex :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataFindIndex context parameters = do
  requireNParameters parameters 0 "dataFindIndex"
  error "Not yet implemented."
  -- TODO


cfDataFindIndices :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfDataFindIndices context parameters = do
  requireNParameters parameters 0 "dataFindIndices"
  error "Not yet implemented."
  -- TODO


cfDataCount :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataCount context parameters = do
  requireNParameters parameters 0 "dataCount"
  error "Not yet implemented."
  -- TODO


cfDataZip :: CustardContext
          -> [CustardValue]
          -> FruitTart (CustardContext, CustardValue)
cfDataZip context parameters = do
  requireNParameters parameters 0 "dataZip"
  error "Not yet implemented."
  -- TODO


cfDataZipWith :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfDataZipWith context parameters = do
  requireNParameters parameters 0 "dataZipWith"
  error "Not yet implemented."
  -- TODO


cfDataUnzip :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfDataUnzip context parameters = do
  requireNParameters parameters 0 "dataUnzip"
  error "Not yet implemented."
  -- TODO


cfDataSort :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataSort context parameters = do
  requireNParameters parameters 0 "dataSort"
  error "Not yet implemented."
  -- TODO


cfDataCopy :: CustardContext
           -> [CustardValue]
           -> FruitTart (CustardContext, CustardValue)
cfDataCopy context parameters = do
  requireNParameters parameters 0 "dataCopy"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Decode :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Decode context parameters = do
  requireNParameters parameters 0 "dataUTF8Decode"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Uncons :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Uncons context parameters = do
  requireNParameters parameters 0 "dataUTF8Uncons"
  error "Not yet implemented."
  -- TODO


cfDataUTF8SplitAt :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfDataUTF8SplitAt context parameters = do
  requireNParameters parameters 0 "dataUTF8SplitAt"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Take :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Take context parameters = do
  requireNParameters parameters 0 "dataUTF8Take"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Drop :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Drop context parameters = do
  requireNParameters parameters 0 "dataUTF8Drop"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Span :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Span context parameters = do
  requireNParameters parameters 0 "dataUTF8Span"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Break :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Break context parameters = do
  requireNParameters parameters 0 "dataUTF8Break"
  error "Not yet implemented."
  -- TODO


cfDataUTF8FromString :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfDataUTF8FromString context parameters = do
  requireNParameters parameters 0 "dataUTF8FromString"
  error "Not yet implemented."
  -- TODO


cfDataUTF8ToString :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfDataUTF8ToString context parameters = do
  requireNParameters parameters 0 "dataUTF8ToString"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Fold :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Fold context parameters = do
  requireNParameters parameters 0 "dataUTF8Fold"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Length :: CustardContext
                 -> [CustardValue]
                 -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Length context parameters = do
  requireNParameters parameters 0 "dataUTF8Length"
  error "Not yet implemented."
  -- TODO


cfDataUTF8Lines :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfDataUTF8Lines context parameters = do
  requireNParameters parameters 0 "dataUTF8Lines"
  error "Not yet implemented."
  -- TODO

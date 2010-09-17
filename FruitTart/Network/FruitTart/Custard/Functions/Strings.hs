module Network.FruitTart.Custard.Functions.Strings (
                                                    cfStringHead,
                                                    cfStringLast,
                                                    cfStringTail,
                                                    cfStringInit,
                                                    cfStringNull,
                                                    cfStringLength,
                                                    cfStringMap,
                                                    cfStringReverse,
                                                    cfStringIntersperse,
                                                    cfStringIntercalate,
                                                    cfStringTranspose,
                                                    cfStringSubsequences,
                                                    cfStringPermutations,
                                                    cfStringFoldl,
                                                    cfStringFoldl1,
                                                    cfStringFoldr,
                                                    cfStringFoldr1,
                                                    cfStringConcat,
                                                    cfStringConcatMap,
                                                    cfStringAny,
                                                    cfStringAll,
                                                    cfStringScanl,
                                                    cfStringScanl1,
                                                    cfStringScanr,
                                                    cfStringScanr1,
                                                    cfStringMapAccumL,
                                                    cfStringMapAccumR,
                                                    cfStringReplicate,
                                                    cfStringUnfoldr,
                                                    cfStringTake,
                                                    cfStringDrop,
                                                    cfStringSplitAt,
                                                    cfStringTakeWhile,
                                                    cfStringDropWhile,
                                                    cfStringSpan,
                                                    cfStringBreak,
                                                    cfStringStripPrefix,
                                                    cfStringGroup,
                                                    cfStringInits,
                                                    cfStringTails,
                                                    cfStringIsPrefixOf,
                                                    cfStringIsSuffixOf,
                                                    cfStringIsInfixOf,
                                                    cfStringElem,
                                                    cfStringNotElem,
                                                    cfStringLookup,
                                                    cfStringFind,
                                                    cfStringFilter,
                                                    cfStringPartition,
                                                    cfStringNth,
                                                    cfStringElemIndex,
                                                    cfStringElemIndices,
                                                    cfStringFindIndex,
                                                    cfStringFindIndices,
                                                    cfStringLines,
                                                    cfStringWords,
                                                    cfStringUnlines,
                                                    cfStringUnwords
                                                   )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
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


cfStringHead :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringHead context parameters = do
  requireNParameters parameters 1 "stringHead"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.decode bytestring of
    Nothing -> error "String is empty."
    Just (result, _) -> return $ CustardCharacter result


cfStringLast :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringLast context parameters = do
  requireNParameters parameters 1 "stringLast"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.length bytestring of
    0 -> error "String is empty."
    nCharacters -> case UTF8.decode $ UTF8.drop (nCharacters - 1) bytestring of
                     Just (result, _) -> return $ CustardCharacter result


cfStringTail :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringTail context parameters = do
  requireNParameters parameters 1 "stringTail"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardString $ UTF8.drop 1 bytestring


cfStringInit :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringInit context parameters = do
  requireNParameters parameters 1 "stringInit"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.length bytestring of
    0 -> return $ CustardString $ BS.empty
    nCharacters -> return $ CustardString
                          $ UTF8.take (nCharacters - 1) bytestring


cfStringNull :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringNull context parameters = do
  requireNParameters parameters 1 "stringNull"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardBool $ BS.null bytestring


cfStringLength :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringLength context parameters = do
  requireNParameters parameters 1 "stringLength"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardInteger $ fromIntegral $ UTF8.length bytestring


cfStringMap :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfStringMap context parameters = do
  error "Not implemented."
  -- TODO


cfStringReverse :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringReverse context parameters = do
  requireNParameters parameters 1 "stringReverse"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardString $ UTF8.fromString $ reverse $ UTF8.toString bytestring


cfStringIntersperse :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfStringIntersperse context parameters = do
  requireNParameters parameters 2 "stringIntersperse"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return $ CustardString $ UTF8.fromString
                         $ intersperse character $ UTF8.toString bytestring


cfStringIntercalate :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfStringIntercalate context parameters = do
  requireNParameters parameters 2 "stringIntercalate"
  bytestring <- valueToUTF8String $ parameters !! 0
  list <- valueToListOfUTF8Strings $ parameters !! 1
  return $ CustardString $ BS.intercalate bytestring list


cfStringTranspose :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringTranspose context parameters = do
  requireNParameters parameters 1 "stringTranspose"
  list <- valueToListOfUTF8Strings $ parameters !! 0
  return $ CustardList $ map CustardString $ BS.transpose list


cfStringSubsequences :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfStringSubsequences context parameters = do
  requireNParameters parameters 1 "stringSubsequences"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardList $ map (CustardString . UTF8.fromString)
                       $ subsequences $ UTF8.toString bytestring


cfStringPermutations :: CustardContext
                     -> [CustardValue]
                     -> FruitTart CustardValue
cfStringPermutations context parameters = do
  requireNParameters parameters 1 "stringPermutations"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardList $ map (CustardString . UTF8.fromString)
                       $ permutations $ UTF8.toString bytestring


cfStringFoldl :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringFoldl context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldl1 :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringFoldl1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldr :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringFoldr context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldr1 :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringFoldr1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcat :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringConcat context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcatMap :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfStringAny :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfStringAny context parameters = do
  error "Not implemented."
  -- TODO


cfStringAll :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfStringAll context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringScanl context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl1 :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringScanr context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr1 :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumL :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumR :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfStringReplicate :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringReplicate context parameters = do
  requireNParameters parameters 2 "stringReplicate"
  count <- valueToInteger $ parameters !! 0
  character <- valueToCharacter $ parameters !! 1
  return $ CustardString $ UTF8.fromString $ genericReplicate count character


cfStringUnfoldr :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfStringTake :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringTake context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return $ CustardString $ UTF8.fromString
                         $ genericTake count $ UTF8.toString bytestring


cfStringDrop :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringDrop context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return $ CustardString $ UTF8.fromString
                         $ genericDrop count $ UTF8.toString bytestring


cfStringSplitAt :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringSplitAt context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  let (before, after) = genericSplitAt count $ UTF8.toString bytestring
  return $ CustardTuple [CustardString $ UTF8.fromString before,
                         CustardString $ UTF8.fromString after]


cfStringTakeWhile :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringDropWhile :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringSpan :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringSpan context parameters = do
  error "Not implemented."
  -- TODO


cfStringBreak :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStringStripPrefix :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfStringStripPrefix context parameters = do
  requireNParameters parameters 2 "stringStripPrefix"
  prefix <- valueToUTF8String $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  case stripPrefix (UTF8.toString prefix) (UTF8.toString bytestring) of
    Nothing -> return $ CustardMaybe Nothing
    Just result -> return $ CustardMaybe $ Just
                          $ CustardString $ UTF8.fromString result


cfStringGroup :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringGroup context parameters = do
  requireNParameters parameters 1 "stringGroup"
  bytestring <- valueToUTF8String $ parameters !! 0
  return $ CustardList $ map (CustardString . UTF8.fromString)
                       $ group $ UTF8.toString bytestring


cfStringInits :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringInits context parameters = do
  error "Not implemented."
  -- TODO


cfStringTails :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringTails context parameters = do
  error "Not implemented."
  -- TODO


cfStringIsPrefixOf :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfStringIsPrefixOf context parameters = do
  requireNParameters parameters 2 "stringIsPrefixOf"
  error "Not implemented."
  -- TODO REALLY
{-
  prefix <- valueToString $ parameters !! 0
  string <- valueToString $ parameters !! 1
  return $ CustardBool $ isPrefixOf prefix string
-}


cfStringIsSuffixOf :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfStringIsSuffixOf context parameters = do
  error "Not implemented."
  -- TODO


cfStringIsInfixOf :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringIsInfixOf context parameters = do
  error "Not implemented."
  -- TODO


cfStringElem :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringElem context parameters = do
  error "Not implemented."
  -- TODO


cfStringNotElem :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringNotElem context parameters = do
  error "Not implemented."
  -- TODO


cfStringLookup :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringLookup context parameters = do
  error "Not implemented."
  -- TODO


cfStringFind :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfStringFind context parameters = do
  error "Not implemented."
  -- TODO


cfStringFilter :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfStringFilter context parameters = do
  error "Not implemented."
  -- TODO


cfStringPartition :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringPartition context parameters = do
  error "Not implemented."
  -- TODO


cfStringNth :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfStringNth context parameters = do
  error "Not implemented."
  -- TODO


cfStringElemIndex :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringElemIndex context parameters = do
  error "Not implemented."
  -- TODO


cfStringElemIndices :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfStringElemIndices context parameters = do
  error "Not implemented."
  -- TODO


cfStringFindIndex :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfStringFindIndices :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfStringFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfStringLines :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringLines context parameters = do
  error "Not implemented."
  -- TODO


cfStringWords :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfStringWords context parameters = do
  error "Not implemented."
  -- TODO


cfStringUnlines :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringUnlines context parameters = do
  error "Not implemented."
  -- TODO


cfStringUnwords :: CustardContext
                -> [CustardValue]
                -> FruitTart CustardValue
cfStringUnwords context parameters = do
  error "Not implemented."
  -- TODO

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
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Custard.Functions.Util
import Network.FruitTart.Common
import Network.FruitTart.Types
import Network.FruitTart.Util


cfStringHead :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringHead context parameters = do
  error "Not implemented."
  -- TODO


cfStringLast :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringLast context parameters = do
  error "Not implemented."
  -- TODO


cfStringTail :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringTail context parameters = do
  error "Not implemented."
  -- TODO


cfStringInit :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringInit context parameters = do
  error "Not implemented."
  -- TODO


cfStringNull :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringNull context parameters = do
  error "Not implemented."
  -- TODO


cfStringLength :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringLength context parameters = do
  requireNParameters parameters 1 "stringLength"
  string <- valueToString $ head parameters
  return $ TemplateInteger $ fromIntegral $ length string


cfStringMap :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfStringMap context parameters = do
  error "Not implemented."
  -- TODO


cfStringReverse :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringReverse context parameters = do
  error "Not implemented."
  -- TODO


cfStringIntersperse :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfStringIntersperse context parameters = do
  error "Not implemented."
  -- TODO


cfStringIntercalate :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfStringIntercalate context parameters = do
  requireNParameters parameters 2 "intercalate"
  string <- valueToString $ head parameters
  list <- valueToList $ head $ drop 1 parameters
  strings <- mapM valueToString list
  return $ CustardString $ intercalate string strings


cfStringTranspose :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringTranspose context parameters = do
  error "Not implemented."
  -- TODO


cfStringSubsequences :: CustardContext
                     -> [AnyCustardValue]
                     -> FruitTart AnyCustardValue
cfStringSubsequences context parameters = do
  error "Not implemented."
  -- TODO


cfStringPermutations :: CustardContext
                     -> [AnyCustardValue]
                     -> FruitTart AnyCustardValue
cfStringPermutations context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldl :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringFoldl context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldl1 :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringFoldl1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldr :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringFoldr context parameters = do
  error "Not implemented."
  -- TODO


cfStringFoldr1 :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringFoldr1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcat :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringConcat context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcatMap :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfStringAny :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfStringAny context parameters = do
  error "Not implemented."
  -- TODO


cfStringAll :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfStringAll context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringScanl context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl1 :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringScanr context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr1 :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumL :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumR :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfStringReplicate :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringReplicate context parameters = do
  error "Not implemented."
  -- TODO


cfStringUnfoldr :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfStringTake :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringTake context parameters = do
  error "Not implemented."
  -- TODO


cfStringDrop :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringDrop context parameters = do
  error "Not implemented."
  -- TODO


cfStringSplitAt :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringSplitAt context parameters = do
  error "Not implemented."
  -- TODO


cfStringTakeWhile :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringDropWhile :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringSpan :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringSpan context parameters = do
  error "Not implemented."
  -- TODO


cfStringBreak :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStringStripPrefix :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfStringStripPrefix context parameters = do
  error "Not implemented."
  -- TODO


cfStringGroup :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringGroup context parameters = do
  error "Not implemented."
  -- TODO


cfStringInits :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringInits context parameters = do
  error "Not implemented."
  -- TODO


cfStringTails :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringTails context parameters = do
  error "Not implemented."
  -- TODO


cfStringIsPrefixOf :: CustardContext
                   -> [AnyCustardValue]
                   -> FruitTart AnyCustardValue
cfStringIsPrefixOf context parameters = do
  error "Not implemented."
  -- TODO


cfStringIsSuffixOf :: CustardContext
                   -> [AnyCustardValue]
                   -> FruitTart AnyCustardValue
cfStringIsSuffixOf context parameters = do
  error "Not implemented."
  -- TODO


cfStringIsInfixOf :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringIsInfixOf context parameters = do
  error "Not implemented."
  -- TODO


cfStringElem :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringElem context parameters = do
  error "Not implemented."
  -- TODO


cfStringNotElem :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringNotElem context parameters = do
  error "Not implemented."
  -- TODO


cfStringLookup :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringLookup context parameters = do
  error "Not implemented."
  -- TODO


cfStringFind :: CustardContext
             -> [AnyCustardValue]
             -> FruitTart AnyCustardValue
cfStringFind context parameters = do
  error "Not implemented."
  -- TODO


cfStringFilter :: CustardContext
               -> [AnyCustardValue]
               -> FruitTart AnyCustardValue
cfStringFilter context parameters = do
  error "Not implemented."
  -- TODO


cfStringPartition :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringPartition context parameters = do
  error "Not implemented."
  -- TODO


cfStringNth :: CustardContext
            -> [AnyCustardValue]
            -> FruitTart AnyCustardValue
cfStringNth context parameters = do
  error "Not implemented."
  -- TODO


cfStringElemIndex :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringElemIndex context parameters = do
  error "Not implemented."
  -- TODO


cfStringElemIndices :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfStringElemIndices context parameters = do
  error "Not implemented."
  -- TODO


cfStringFindIndex :: CustardContext
                  -> [AnyCustardValue]
                  -> FruitTart AnyCustardValue
cfStringFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfStringFindIndices :: CustardContext
                    -> [AnyCustardValue]
                    -> FruitTart AnyCustardValue
cfStringFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfStringLines :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringLines context parameters = do
  error "Not implemented."
  -- TODO


cfStringWords :: CustardContext
              -> [AnyCustardValue]
              -> FruitTart AnyCustardValue
cfStringWords context parameters = do
  error "Not implemented."
  -- TODO


cfStringUnlines :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringUnlines context parameters = do
  error "Not implemented."
  -- TODO


cfStringUnwords :: CustardContext
                -> [AnyCustardValue]
                -> FruitTart AnyCustardValue
cfStringUnwords context parameters = do
  error "Not implemented."
  -- TODO

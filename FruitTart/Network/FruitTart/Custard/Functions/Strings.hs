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
                                                    cfStringFold,
                                                    cfStringFold1,
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
import Data.Char
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
             -> FruitTart (CustardContext, CustardValue)
cfStringHead context parameters = do
  requireNParameters parameters 1 "stringHead"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.decode bytestring of
    Nothing -> error "String is empty."
    Just (result, _) -> return (context, CustardCharacter result)


cfStringLast :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringLast context parameters = do
  requireNParameters parameters 1 "stringLast"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.length bytestring of
    0 -> error "String is empty."
    nCharacters -> case UTF8.decode $ UTF8.drop (nCharacters - 1) bytestring of
                     Just (result, _) -> return (context,
                                                 CustardCharacter result)


cfStringTail :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringTail context parameters = do
  requireNParameters parameters 1 "stringTail"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context, CustardString $ UTF8.drop 1 bytestring)


cfStringInit :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringInit context parameters = do
  requireNParameters parameters 1 "stringInit"
  bytestring <- valueToUTF8String $ parameters !! 0
  case UTF8.length bytestring of
    0 -> return (context, CustardString $ BS.empty)
    nCharacters -> return (context,
                           CustardString
                            $ UTF8.take (nCharacters - 1) bytestring)


cfStringNull :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringNull context parameters = do
  requireNParameters parameters 1 "stringNull"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context, CustardBool $ BS.null bytestring)


cfStringLength :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfStringLength context parameters = do
  requireNParameters parameters 1 "stringLength"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context, CustardInteger $ fromIntegral $ UTF8.length bytestring)


cfStringMap :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfStringMap context parameters = do
  requireNParameters parameters 2 "stringMap"
  action <- valueToMonadicAction $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  (context, result)
    <- foldM (\(context, resultSoFar) character -> do
                (context, character)
                  <- action context [CustardCharacter character]
                character <- valueToCharacter character
                return (context, resultSoFar ++ [character]))
             (context, [])
             $ UTF8.toString bytestring
  return (context, CustardString $ UTF8.fromString result)


cfStringReverse :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringReverse context parameters = do
  requireNParameters parameters 1 "stringReverse"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardString $ UTF8.fromString $ reverse $ UTF8.toString bytestring)


cfStringIntersperse :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfStringIntersperse context parameters = do
  requireNParameters parameters 2 "stringIntersperse"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardString $ UTF8.fromString
                        $ intersperse character $ UTF8.toString bytestring)


cfStringIntercalate :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfStringIntercalate context parameters = do
  requireNParameters parameters 2 "stringIntercalate"
  bytestring <- valueToUTF8String $ parameters !! 0
  list <- valueToListOfUTF8Strings $ parameters !! 1
  return (context, CustardString $ BS.intercalate bytestring list)


cfStringTranspose :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringTranspose context parameters = do
  requireNParameters parameters 1 "stringTranspose"
  list <- valueToListOfUTF8Strings $ parameters !! 0
  return (context, CustardList $ map CustardString $ BS.transpose list)


cfStringSubsequences :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfStringSubsequences context parameters = do
  requireNParameters parameters 1 "stringSubsequences"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardList $ map (CustardString . UTF8.fromString)
                            $ subsequences $ UTF8.toString bytestring)


cfStringPermutations :: CustardContext
                     -> [CustardValue]
                     -> FruitTart (CustardContext, CustardValue)
cfStringPermutations context parameters = do
  requireNParameters parameters 1 "stringPermutations"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardList $ map (CustardString . UTF8.fromString)
                            $ permutations $ UTF8.toString bytestring)


cfStringFold :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringFold context parameters = do
  requireNParameters parameters 3 "stringMap"
  action <- valueToMonadicAction $ parameters !! 0
  let initialValue = parameters !! 1
  bytestring <- valueToUTF8String $ parameters !! 2
  (context, result)
    <- foldM (\(context, resultSoFar) character -> do
                (context, resultSoFar)
                  <- action context [resultSoFar, CustardCharacter character]
                return (context, resultSoFar))
             (context, initialValue)
             $ UTF8.toString bytestring
  return (context, result)


cfStringFold1 :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringFold1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcat :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfStringConcat context parameters = do
  error "Not implemented."
  -- TODO


cfStringConcatMap :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringConcatMap context parameters = do
  error "Not implemented."
  -- TODO


cfStringAny :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfStringAny context parameters = do
  error "Not implemented."
  -- TODO


cfStringAll :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfStringAll context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringScanl context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanl1 :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfStringScanl1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringScanr context parameters = do
  error "Not implemented."
  -- TODO


cfStringScanr1 :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfStringScanr1 context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumL :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringMapAccumL context parameters = do
  error "Not implemented."
  -- TODO


cfStringMapAccumR :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringMapAccumR context parameters = do
  error "Not implemented."
  -- TODO


cfStringReplicate :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringReplicate context parameters = do
  requireNParameters parameters 2 "stringReplicate"
  count <- valueToInteger $ parameters !! 0
  character <- valueToCharacter $ parameters !! 1
  return (context,
          CustardString $ UTF8.fromString $ genericReplicate count character)


cfStringUnfoldr :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringUnfoldr context parameters = do
  error "Not implemented."
  -- TODO


cfStringTake :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringTake context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardString $ UTF8.fromString
                           $ genericTake count $ UTF8.toString bytestring)


cfStringDrop :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringDrop context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardString $ UTF8.fromString
                           $ genericDrop count $ UTF8.toString bytestring)


cfStringSplitAt :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringSplitAt context parameters = do
  requireNParameters parameters 2 "stringTake"
  count <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  let (before, after) = genericSplitAt count $ UTF8.toString bytestring
  return (context,
          CustardTuple [CustardString $ UTF8.fromString before,
                        CustardString $ UTF8.fromString after])


cfStringTakeWhile :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringTakeWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringDropWhile :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringDropWhile context parameters = do
  error "Not implemented."
  -- TODO


cfStringSpan :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringSpan context parameters = do
  error "Not implemented."
  -- TODO


cfStringBreak :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringBreak context parameters = do
  error "Not implemented."
  -- TODO


cfStringStripPrefix :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfStringStripPrefix context parameters = do
  requireNParameters parameters 2 "stringStripPrefix"
  prefix <- valueToUTF8String $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  case stripPrefix (UTF8.toString prefix) (UTF8.toString bytestring) of
    Nothing -> return (context, CustardMaybe Nothing)
    Just result -> return (context,
                           CustardMaybe $ Just
                            $ CustardString $ UTF8.fromString result)


cfStringGroup :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringGroup context parameters = do
  requireNParameters parameters 1 "stringGroup"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardList $ map (CustardString . UTF8.fromString)
                            $ group $ UTF8.toString bytestring)


cfStringInits :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringInits context parameters = do
  requireNParameters parameters 1 "stringInits"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardList $ map (CustardString . UTF8.fromString)
                            $ inits $ UTF8.toString bytestring)


cfStringTails :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringTails context parameters = do
  requireNParameters parameters 1 "stringInits"
  bytestring <- valueToUTF8String $ parameters !! 0
  return (context,
          CustardList $ map (CustardString . UTF8.fromString)
                            $ tails $ UTF8.toString bytestring)


cfStringIsPrefixOf :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfStringIsPrefixOf context parameters = do
  requireNParameters parameters 2 "stringIsPrefixOf"
  bytestringA <- valueToUTF8String $ parameters !! 0
  bytestringB <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardBool $ isPrefixOf (UTF8.toString bytestringA)
                                   (UTF8.toString bytestringB))


cfStringIsSuffixOf :: CustardContext
                   -> [CustardValue]
                   -> FruitTart (CustardContext, CustardValue)
cfStringIsSuffixOf context parameters = do
  requireNParameters parameters 2 "stringIsSuffixOf"
  bytestringA <- valueToUTF8String $ parameters !! 0
  bytestringB <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardBool $ isSuffixOf (UTF8.toString bytestringA)
                                   (UTF8.toString bytestringB))


cfStringIsInfixOf :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringIsInfixOf context parameters = do
  requireNParameters parameters 2 "stringIsInfixOf"
  bytestringA <- valueToUTF8String $ parameters !! 0
  bytestringB <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardBool $ isInfixOf (UTF8.toString bytestringA)
                                  (UTF8.toString bytestringB))


cfStringElem :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringElem context parameters = do
  requireNParameters parameters 2 "stringElem"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardBool $ elem character $ UTF8.toString bytestring)


cfStringNotElem :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringNotElem context parameters = do
  requireNParameters parameters 2 "stringNotElem"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardBool $ notElem character $ UTF8.toString bytestring)


cfStringFind :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfStringFind context parameters = do
  error "Not implemented."
  -- TODO


cfStringFilter :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfStringFilter context parameters = do
  error "Not implemented."
  -- TODO


cfStringPartition :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringPartition context parameters = do
  error "Not implemented."
  -- TODO


cfStringNth :: CustardContext
            -> [CustardValue]
            -> FruitTart (CustardContext, CustardValue)
cfStringNth context parameters = do
  requireNParameters parameters 2 "stringNth"
  n <- valueToInteger $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  case UTF8.decode $ UTF8.drop (fromIntegral n) bytestring of
    Nothing -> error "String is too short."
    Just (result, _) -> return (context, CustardCharacter result)


cfStringElemIndex :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringElemIndex context parameters = do
  requireNParameters parameters 2 "stringElemIndex"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  case elemIndex character $ UTF8.toString bytestring of
    Nothing -> return (context, CustardMaybe Nothing)
    Just result -> return (context,
                           CustardMaybe $ Just $ CustardInteger
                                                  $ fromIntegral result)


cfStringElemIndices :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfStringElemIndices context parameters = do
  requireNParameters parameters 2 "stringElemIndices"
  character <- valueToCharacter $ parameters !! 0
  bytestring <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardList $ map (CustardInteger . fromIntegral)
                            $ elemIndices character $ UTF8.toString bytestring)


cfStringFindIndex :: CustardContext
                  -> [CustardValue]
                  -> FruitTart (CustardContext, CustardValue)
cfStringFindIndex context parameters = do
  error "Not implemented."
  -- TODO


cfStringFindIndices :: CustardContext
                    -> [CustardValue]
                    -> FruitTart (CustardContext, CustardValue)
cfStringFindIndices context parameters = do
  error "Not implemented."
  -- TODO


cfStringLines :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringLines context parameters = do
  requireNParameters parameters 1 "stringLines"
  bytestring <- valueToUTF8String $ parameters !! 0
  (lastLine, linesSoFar)
    <- foldM (\(bytestring, linesSoFar) index -> do
                let before = UTF8.take index bytestring
                    after = UTF8.drop (index + 1) bytestring
                return (after, linesSoFar ++ [before]))
             (bytestring, [])
             $ elemIndices '\n' $ UTF8.toString bytestring
  let lines = linesSoFar ++ [lastLine]
  return (context, CustardList $ map CustardString lines)


cfStringWords :: CustardContext
              -> [CustardValue]
              -> FruitTart (CustardContext, CustardValue)
cfStringWords context parameters = do
  requireNParameters parameters 1 "stringWords"
  bytestring <- valueToUTF8String $ parameters !! 0
  (lastWord, wordsSoFar)
    <- foldM (\(bytestring, wordsSoFar) index -> do
                let before = UTF8.take index bytestring
                    after = UTF8.drop (index + 1) bytestring
                return (after, wordsSoFar ++ [before]))
             (bytestring, [])
             $ findIndices isSpace $ UTF8.toString bytestring
  let words = wordsSoFar ++ [lastWord]
      words' = filter (\line -> not $ BS.null line) words
  return (context, CustardList $ map CustardString words')


cfStringUnlines :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringUnlines context parameters = do
  requireNParameters parameters 1 "stringUnlines"
  lines <- valueToListOfUTF8Strings $ parameters !! 0
  return (context, CustardString $ BS.intercalate (UTF8.fromString "\n") lines)


cfStringUnwords :: CustardContext
                -> [CustardValue]
                -> FruitTart (CustardContext, CustardValue)
cfStringUnwords context parameters = do
  requireNParameters parameters 1 "stringUnwords"
  words <- valueToListOfUTF8Strings $ parameters !! 0
  return (context, CustardString $ BS.intercalate (UTF8.fromString " ") words)

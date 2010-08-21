module Network.FruitTart.Custard.Functions.General (
                                                    cfJust,
                                                    cfParameter,
                                                    cfIsNothing,
                                                    cfIsJust,
                                                    cfFromJust,
                                                    cfStringWordCount,
                                                    cfCompareIntegers,
                                                    cfShowInteger,
                                                    cfShowBool,
                                                    cfByteSizeToString,
                                                    cfTimestampToString,
                                                    cfEscapeAttribute,
                                                    cfEscapeHTML,
                                                    cfNewlinesToParagraphs
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


cfJust :: CustardContext
       -> [CustardValue]
       -> FruitTart CustardValue
cfJust context parameters = do
  requireNParameters parameters 1 "just"
  return $ CustardMaybe $ Just $ head parameters


cfParameter :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfParameter context parameters = do
  requireNParameters parameters 1 "parameter"
  n <- valueToInteger $ head parameters
  let CustardContext { custardContextParameters = contextParameters } = context
  if n < (fromIntegral $ length contextParameters)
    then return $ head $ drop (fromIntegral n) contextParameters
    else error $ "Too few parameters for parameter(" ++ (show n) ++ ")."


cfIsNothing :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfIsNothing context parameters = do
  requireNParameters parameters 1 "isNothing"
  value <- return $ head parameters
  return $ case value of
             CustardMaybe Nothing -> CustardBool True
             CustardMaybe (Just _) -> CustardBool False
             _ -> error $ "Parameter is not a Maybe in isNothing()."


cfIsJust :: CustardContext
         -> [CustardValue]
         -> FruitTart CustardValue
cfIsJust context parameters = do
  requireNParameters parameters 1 "isJust"
  value <- return $ head parameters
  return $ case value of
             CustardMaybe Nothing -> CustardBool False
             CustardMaybe (Just _) -> CustardBool True
             _ -> error $ "Parameter is not a Maybe in isJust()."


cfFromJust :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfFromJust context parameters = do
  requireNParameters parameters 1 "fromJust"
  value <- return $ head parameters
  return $ case value of
             CustardMaybe Nothing
                 -> error $ "Parameter is nothing in fromJust()."
             CustardMaybe (Just result) -> result
             _ -> error $ "Parameter is not a Maybe in fromJust()."


cfStringWordCount :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfStringWordCount context parameters = do
  requireNParameters parameters 1 "stringWordCount"
  string <- valueToString $ head parameters
  let wordCount "" = 0
      wordCount string = case elemIndex ' ' string of
                           Nothing -> 1
                           Just index -> let (_, rest) = splitAt index string
                                         in 1 + wordCount (drop 1 rest)
  return $ CustardInteger $ fromIntegral $ wordCount string


cfCompareIntegers :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfCompareIntegers context parameters = do
  requireNParameters parameters 2 "compareIntegers"
  a <- valueToInteger $ head parameters
  b <- valueToInteger $ head $ drop 1 parameters
  return $ CustardOrdering $ compare a b


cfShowInteger :: CustardContext
              -> [CustardValue]
              -> FruitTart CustardValue
cfShowInteger context parameters = do
  requireNParameters parameters 1 "showInteger"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ show integer


cfShowBool :: CustardContext
           -> [CustardValue]
           -> FruitTart CustardValue
cfShowBool context parameters = do
  requireNParameters parameters 1 "showBool"
  bool <- valueToBoolean $ head parameters
  return $ CustardString $ show bool


cfByteSizeToString :: CustardContext
                   -> [CustardValue]
                   -> FruitTart CustardValue
cfByteSizeToString context parameters = do
  requireNParameters parameters 1 "byteSizeToString"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ byteSizeToString integer


cfTimestampToString :: CustardContext
                    -> [CustardValue]
                    -> FruitTart CustardValue
cfTimestampToString context parameters = do
  requireNParameters parameters 1 "timestampToString"
  integer <- valueToInteger $ head parameters
  return $ CustardString $ timestampToString integer


cfEscapeAttribute :: CustardContext
                  -> [CustardValue]
                  -> FruitTart CustardValue
cfEscapeAttribute context parameters = do
  requireNParameters parameters 1 "escapeAttribute"
  string <- valueToString $ head parameters
  return $ CustardString $ escapeAttribute string


cfEscapeHTML :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfEscapeHTML context parameters = do
  requireNParameters parameters 1 "escapeHTML"
  string <- valueToString $ head parameters
  return $ CustardString $ escapeHTML string


cfNewlinesToParagraphs :: CustardContext
                       -> [CustardValue]
                       -> FruitTart CustardValue
cfNewlinesToParagraphs context parameters = do
  requireNParameters parameters 1 "newlinesToParagraphs"
  string <- valueToString $ head parameters
  return $ CustardString $ newlinesToParagraphs string

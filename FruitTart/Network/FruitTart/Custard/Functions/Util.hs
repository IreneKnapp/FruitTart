{-# LANGUAGE GADTs #-}
module Network.FruitTart.Custard.Functions.Util
  (
   valueToBoolean,
   valueToInteger,
   valueToCharacter,
   valueToString,
   valueToListOfMaps,
   typecheckAndHomogenizeList,
   errorNotAList,
   errorNotAListOfBooleans,
   errorNotAListOfIntegers,
   errorNotAListOfLists,
   errorNotAListOfIntegersOrCharacters,
   errorListEmpty,
   errorListHeterogeneous,
   requireControllerContext,
   requireNParameters
  )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.ByteString hiding (concat, index, length, map)
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Common
import Network.FruitTart.Types
import Network.FruitTart.Util


valueToBoolean :: AnyCustardValue -> FruitTart Bool
valueToBoolean (SomeCustardValue (CustardBool boolean)) = return boolean
valueToBoolean value = error $ "Value is not a Boolean."


valueToInteger :: AnyCustardValue -> FruitTart Int64
valueToInteger (SomeCustardValue (CustardInteger integer)) = return integer
valueToInteger value = error $ "Value is not an Integer."


valueToCharacter :: AnyCustardValue -> FruitTart Char
valueToCharacter (SomeCustardValue (CustardCharacter char)) = return char
valueToCharacter value = error $ "Value is not a Character."


valueToString :: AnyCustardValue -> FruitTart String
valueToString (SomeCustardValue (CustardString string)) = return string
valueToString value = error $ "Value is not a String."


valueToListOfMaps :: AnyCustardValue
                  -> FruitTart [Map (String, String) AnyCustardValue]
valueToListOfMaps (SomeCustardValue (CustardList [])) = return []
valueToListOfMaps (SomeCustardValue (CustardList maps@(CustardMap _ : _)))
  = return $ map ((\(CustardMap map) -> map)
                  :: CustardValue (Map (String, String) AnyCustardValue)
                  -> Map (String, String) AnyCustardValue)
                 maps
valueToListOfMaps value = error $ "Value is not a List of Maps."


typecheckAndHomogenizeList :: [AnyCustardValue] -> FruitTart AnyCustardValue
typecheckAndHomogenizeList [] = return $ SomeCustardValue $ CustardList []
typecheckAndHomogenizeList items@(SomeCustardValue CustardNull : _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue ()]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardNull):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardBool _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue Bool]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardBool _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardInteger _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue Int64]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardInteger _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardCharacter _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue Char]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardCharacter _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardString _): _) = do
  let processFromHere :: [AnyCustardValue]
                      -> FruitTart [CustardValue CustardStringTypeWitness]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardString _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardList _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue [a]]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardList _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardMaybe _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue (Maybe a)]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardMaybe _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardOrdering _): _) = do
  let processFromHere :: [AnyCustardValue] -> FruitTart [CustardValue Ordering]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardOrdering _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardMap _): _) = do
  let processFromHere :: [AnyCustardValue]
                      -> FruitTart [CustardValue (Map (String, String)
                                                      AnyCustardValue)]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardMap _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardLambda _ _ _): _) = do
  let processFromHere :: [AnyCustardValue]
                      -> FruitTart [CustardValue CustardLambdaTypeWitness]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardLambda _ _ _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere (SomeCustardValue value@(CustardNativeLambda _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList
typecheckAndHomogenizeList items@(SomeCustardValue (CustardNativeLambda _): _) = do
  let processFromHere :: [AnyCustardValue]
                      -> FruitTart [CustardValue CustardLambdaTypeWitness]
      processFromHere [] = return []
      processFromHere (SomeCustardValue value@(CustardLambda _ _ _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere (SomeCustardValue value@(CustardNativeLambda _):rest) = do
        rest <- processFromHere rest
        return (value:rest)
      processFromHere _ = errorListHeterogeneous
  processFromHere items >>= return . SomeCustardValue . CustardList


errorNotAList :: FruitTart a
errorNotAList = error $ "Value is not a List."


errorNotAListOfBooleans :: FruitTart a
errorNotAListOfBooleans = error $ "Value is not a List of Booleans."


errorNotAListOfIntegers :: FruitTart a
errorNotAListOfIntegers = error $ "Value is not a List of Integers."


errorNotAListOfLists :: FruitTart a
errorNotAListOfLists = error $ "Value is not a List of Lists."


errorNotAListOfIntegersOrCharacters :: FruitTart a
errorNotAListOfIntegersOrCharacters
  = error $ "Value is not a List of Integers or Characters."


errorListEmpty :: FruitTart a
errorListEmpty = error $ "Value is an empty List."


errorListHeterogeneous :: FruitTart a
errorListHeterogeneous = error $ "Values are not all of the same type."


requireControllerContext :: CustardContext -> String -> FruitTart ()
requireControllerContext context functionName = do
  let CustardContext { custardContextType = contextType } = context
  case contextType of
    ControllerContext -> return ()
    _ -> error $ functionName ++ "() can only be used in controllers."


requireNParameters :: [AnyCustardValue] -> Int -> String -> FruitTart ()
requireNParameters parameters n functionName = do
  if length parameters /= n
    then error $ "Invalid number of parameters to " ++ functionName ++ "()."
    else return ()

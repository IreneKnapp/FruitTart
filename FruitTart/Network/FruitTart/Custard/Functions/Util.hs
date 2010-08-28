{-# LANGUAGE GADTs #-}
module Network.FruitTart.Custard.Functions.Util
  (
   valueToSymbol,
   valueToBoolean,
   valueToInteger,
   valueToCharacter,
   valueToString,
   valueToListOfStrings,
   valueToListOfMaps,
   valueToMaybeString,
   valueToMaybeInteger,
   valueToMap,
   valueToByteString,
   valueToHTTPHeader,
   valueToHTTPCookie,
   typecheckList,
   errorNotAList,
   errorNotAListOfBooleans,
   errorNotAListOfIntegers,
   errorNotAListOfStrings,
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

import qualified Network.FastCGI as FCGI

import Network.FruitTart.Custard.Syntax
import Network.FruitTart.Types
import Network.FruitTart.Util


valueToSymbol :: CustardValue -> FruitTart (String, String)
valueToSymbol (CustardSymbol moduleName properName)
  = return (moduleName, properName)
valueToSymbol value = error $ "Value is not a Symbol."


valueToBoolean :: CustardValue -> FruitTart Bool
valueToBoolean (CustardBool boolean) = return boolean
valueToBoolean value = error $ "Value is not a Boolean."


valueToInteger :: CustardValue -> FruitTart Int64
valueToInteger (CustardInteger integer) = return integer
valueToInteger value = error $ "Value is not an Integer."


valueToCharacter :: CustardValue -> FruitTart Char
valueToCharacter (CustardCharacter char) = return char
valueToCharacter value = error $ "Value is not a Character."


valueToString :: CustardValue -> FruitTart String
valueToString (CustardString string) = return string
valueToString value = error $ "Value is not a String."


valueToListOfStrings :: CustardValue
                     -> FruitTart [String]
valueToListOfStrings (CustardList []) = return []
valueToListOfStrings (CustardList items@(CustardString _ : _)) =
  mapM (\(CustardString string) -> return string) items
valueToListOfStrings value = errorNotAListOfStrings


valueToListOfMaps :: CustardValue
                  -> FruitTart [Map (String, String) CustardValue]
valueToListOfMaps (CustardList []) = return []
valueToListOfMaps (CustardList maps@(CustardMap _ : _))
  = return $ map (\(CustardMap map) -> map)
                 maps
valueToListOfMaps value = error $ "Value is not a List of Maps."


valueToMaybeString :: CustardValue -> FruitTart (Maybe String)
valueToMaybeString (CustardMaybe Nothing) = return Nothing
valueToMaybeString (CustardMaybe (Just (CustardString string)))
  = return $ Just string
valueToMaybeString value = error "Value is not a Maybe containing a String."


valueToMaybeInteger :: CustardValue -> FruitTart (Maybe Int64)
valueToMaybeInteger (CustardMaybe Nothing) = return Nothing
valueToMaybeInteger (CustardMaybe (Just (CustardInteger integer)))
  = return $ Just integer
valueToMaybeInteger value = error "Value is not a Maybe containing an Integer."


valueToMap :: CustardValue
           -> FruitTart (Map (String, String) CustardValue)
valueToMap (CustardMap theMap) = return theMap
valueToMap value = error $ "Value is not a Map."


valueToByteString :: CustardValue
                  -> FruitTart ByteString
valueToByteString (CustardData byteString) = return byteString
valueToByteString value = error $ "Value is not Data."


valueToHTTPHeader :: CustardValue
                  -> FruitTart FCGI.Header
valueToHTTPHeader (CustardHTTPHeader header) = return header
valueToHTTPHeader value = error $ "Value is not an HTTPHeader."


valueToHTTPCookie :: CustardValue
                 -> FruitTart FCGI.Cookie
valueToHTTPCookie (CustardHTTPCookie cookie) = return cookie
valueToHTTPCookie value = error $ "Value is not an HTTPCookie."


typecheckList :: [CustardValue] -> FruitTart ()
typecheckList items = do
  let process _ [] = return ()
      process Nothing (item@CustardNull:rest)
        = process (Just item) rest
      process witness@(Just CustardNull)
              (item@CustardNull:rest)
        = process witness rest
      process Nothing (item@(CustardBool _):rest)
        = process (Just item) rest
      process witness@(Just (CustardBool _))
              (item@(CustardBool _):rest)
        = process witness rest
      process Nothing (item@(CustardInteger _):rest)
        = process (Just item) rest
      process witness@(Just (CustardInteger _))
              (item@(CustardInteger _):rest)
        = process witness rest
      process Nothing (item@(CustardCharacter _):rest)
        = process (Just item) rest
      process witness@(Just (CustardCharacter _))
              (item@(CustardCharacter _):rest)
        = process witness rest
      process Nothing (item@(CustardString _):rest)
        = process (Just item) rest
      process witness@(Just (CustardString _))
              (item@(CustardString _):rest)
        = process witness rest
      process Nothing (item@(CustardList _):rest)
        = process (Just item) rest
      process witness@(Just (CustardList _))
              (item@(CustardList _):rest)
        = process witness rest
      process Nothing (item@(CustardMaybe _):rest)
        = process (Just item) rest
      process witness@(Just (CustardMaybe _))
              (item@(CustardMaybe _):rest)
        = process witness rest
      process Nothing (item@(CustardOrdering _):rest)
        = process (Just item) rest
      process witness@(Just (CustardOrdering _))
              (item@(CustardOrdering _):rest)
        = process witness rest
      process Nothing (item@(CustardMap _):rest)
        = process (Just item) rest
      process witness@(Just (CustardMap _))
              (item@(CustardMap _):rest)
        = process witness rest
      process Nothing (item@(CustardLambda _ _ _ _):rest)
        = process (Just item) rest
      process witness@(Just (CustardLambda _ _ _ _))
              (item@(CustardLambda _ _ _ _):rest)
        = process witness rest
      process witness@(Just (CustardLambda _ _ _ _))
              (item@(CustardNativeLambda _ _):rest)
        = process witness rest
      process Nothing (item@(CustardNativeLambda _ _):rest)
        = process (Just item) rest
      process witness@(Just (CustardNativeLambda _ _))
              (item@(CustardLambda _ _ _ _):rest)
        = process witness rest
      process witness@(Just (CustardNativeLambda _ _))
              (item@(CustardNativeLambda _ _):rest)
        = process witness rest
      process _ _ = errorListHeterogeneous
  process Nothing items


errorNotAList :: FruitTart a
errorNotAList = error $ "Value is not a List."


errorNotAListOfBooleans :: FruitTart a
errorNotAListOfBooleans = error $ "Value is not a List of Booleans."


errorNotAListOfIntegers :: FruitTart a
errorNotAListOfIntegers = error $ "Value is not a List of Integers."


errorNotAListOfStrings :: FruitTart a
errorNotAListOfStrings = error $ "Value is not a List of Strings."


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


requireNParameters :: [CustardValue] -> Int -> String -> FruitTart ()
requireNParameters parameters n functionName = do
  if length parameters /= n
    then error $ "Invalid number of parameters to " ++ functionName ++ "()."
    else return ()

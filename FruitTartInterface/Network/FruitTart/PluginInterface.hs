module Network.FruitTart.PluginInterface
    (
     -- Data.Dynamic
     toDyn,
     
     -- PluginInterface
     ActionTable,
     ControllerTable,
     ParameterType(..),
     Interface(..),
     makeActionTable,
     combineActionTables
    )
    where

import Control.Concurrent.MVar
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.IO.Unsafe

import Database.SQLite3


type ActionTable = Map String
                       (Map String
                            ([ParameterType],
                             [(String, ParameterType)],
                             Dynamic))
type ControllerTable = Map String ActionTable

data ParameterType = IDParameter
                   | StringParameter
                   | EitherStringIDParameter
                     deriving (Eq)


data Interface = Interface {
      interfaceVersion :: Integer,
      interfaceDispatchTable :: ControllerTable,
      interfaceModuleName :: String,
      interfaceModuleVersion :: Int64,
      interfaceModuleSchemaVersion :: Int64,
      interfacePrerequisites :: [(String, Int64)],
      interfaceInitDatabase :: Database -> IO (),
      interfaceInitState :: IO Dynamic
    }


makeActionTable
    :: [(String, String, [ParameterType], [(String, ParameterType)], Dynamic)]
    -> ActionTable
makeActionTable allActions
    = Map.fromList
      $ map (\actionsWithName@((name, _, _, _, _):_)
             -> (name,
                 Map.fromList
                 $ map (\(_, method, parameters, namedParameters, function)
                        -> (method, (parameters, namedParameters, function)))
                       actionsWithName))
            $ groupBy (\(nameA, _, _, _, _) (nameB, _, _, _, _) -> nameA == nameB)
                      $ sortBy (\(nameA, methodA, _, _, _) (nameB, methodB, _, _, _)
                                -> let compareNames = compare nameA nameB
                                       compareMethods = compare methodA methodB
                                   in if compareNames == EQ
                                        then compareMethods
                                        else compareNames)
                               allActions


combineActionTables :: [(String, ActionTable)] -> ControllerTable
combineActionTables = Map.fromList

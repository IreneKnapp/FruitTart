module Network.FruitTart.PluginInterface
    (
     -- Data.Dynamic
     toDyn,
     
     -- PluginInterface
     ActionTable,
     ControllerTable,
     ParameterType(..),
     FunctionTable,
     CombinedFunctionTable,
     Interface(..),
     makeActionTable,
     makeFunctionTable,
     combineActionTables,
     combineFunctionTables
    )
    where

import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

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

type FunctionTable = Map String Dynamic
type CombinedFunctionTable = Map (String, String) Dynamic


data Interface = Interface {
      interfaceVersion :: Integer,
      interfaceDispatchTable :: ControllerTable,
      interfaceFunctionTable :: CombinedFunctionTable,
      interfaceModuleName :: String,
      interfaceModuleVersion :: Integer,
      interfacePrerequisites :: [(String, Integer)],
      interfaceInitDatabase :: Database -> IO ()
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
                      allActions


makeFunctionTable :: [(String, Dynamic)] -> FunctionTable
makeFunctionTable = Map.fromList


combineActionTables :: [(String, ActionTable)] -> ControllerTable
combineActionTables = Map.fromList


combineFunctionTables :: [(String, FunctionTable)] -> CombinedFunctionTable
combineFunctionTables functionTables
    = Map.fromList
      $ concat
      $ map (\(moduleName, table)
             -> map (\(functionName, function)
                     -> ((moduleName, functionName), function))
                    $ Map.toList table)
            functionTables

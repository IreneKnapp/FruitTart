module Network.FruitTart.PluginInterface
    (
     ActionTable,
     ControllerTable,
     ParameterType(..),
     Interface(..),
     requireModuleVersion
    )
    where

import Data.Dynamic
import Data.Map (Map)

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
      dispatchTable :: ControllerTable,
      initDatabase :: Database -> IO Bool
    }

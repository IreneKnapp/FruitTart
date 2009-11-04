module FruitTartInterface (Interface(..)) where

import Data.Map (Map)

import Database.SQLite3


type ActionTable = Map String
                       (Map String
                            ([ParameterType],
                             [(String, ParameterType)],
                             Dynamic))
type ControllerTable = Map String ActionTable

data Interface = Interface {
      controllerTable :: ControllerTable,
      initDatabase :: Database -> IO Bool
    }

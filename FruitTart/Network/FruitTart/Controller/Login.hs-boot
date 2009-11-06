module Network.FruitTart.Controller.Login (actionTable,
       					   functionTable,
                                           getLoggedInUserID,
                                           getEffectiveUserID,
                                           outputMustLoginPage)
    where

import Data.Int

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


actionTable :: ActionTable
functionTable :: FunctionTable
getLoggedInUserID :: FruitTart (Maybe Int64)
getEffectiveUserID :: FruitTart Int64
outputMustLoginPage :: String -> FruitTart CGIResult

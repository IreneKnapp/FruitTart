module Network.FruitTart.Controller.Login (actionTable,
                                           getLoggedInUser,
                                           getEffectiveUser,
                                           outputMustLoginPage)
    where

import Data.Int

import Network.FruitTart.Util


actionTable :: ActionTable
getLoggedInUser :: FruitTart (Maybe Int64)
getEffectiveUser :: FruitTart Int64
outputMustLoginPage :: String -> FruitTart CGIResult

module Network.FruitTart.Captcha.Imports (
                                          importFunctionTableMVar
                                         )
    where

import Control.Concurrent.MVar
import System.IO.Unsafe

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


importFunctionTableMVar :: MVar CombinedFunctionTable
importFunctionTableMVar = unsafePerformIO newEmptyMVar

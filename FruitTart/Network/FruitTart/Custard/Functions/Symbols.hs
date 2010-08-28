module Network.FruitTart.Custard.Functions.Symbols (
                                                    cfSymbolName,
                                                    cfSymbolModule,
                                                    cfMakeSymbol
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


cfSymbolName :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfSymbolName context parameters = do
  requireNParameters parameters 1 "symbolName"
  (_, properName) <- valueToSymbol $ parameters !! 0
  return $ CustardString properName


cfSymbolModule :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfSymbolModule context parameters = do
  requireNParameters parameters 1 "symbolModule"
  (moduleName, _) <- valueToSymbol $ parameters !! 0
  return $ CustardString moduleName


cfMakeSymbol :: CustardContext
             -> [CustardValue]
             -> FruitTart CustardValue
cfMakeSymbol context parameters = do
  requireNParameters parameters 2 "makeSymbol"
  moduleName <- valueToString $ parameters !! 0
  properName <- valueToString $ parameters !! 1
  return $ CustardSymbol moduleName properName

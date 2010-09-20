module Network.FruitTart.Custard.Functions.Symbols (
                                                    cfSymbolName,
                                                    cfSymbolModule,
                                                    cfMakeSymbol
                                                   )
  where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.UTF8 as UTF8
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
             -> FruitTart (CustardContext, CustardValue)
cfSymbolName context parameters = do
  requireNParameters parameters 1 "symbolName"
  (_, properName) <- valueToSymbol $ parameters !! 0
  return (context, CustardString $ UTF8.fromString properName)


cfSymbolModule :: CustardContext
               -> [CustardValue]
               -> FruitTart (CustardContext, CustardValue)
cfSymbolModule context parameters = do
  requireNParameters parameters 1 "symbolModule"
  (moduleName, _) <- valueToSymbol $ parameters !! 0
  return (context, CustardString $ UTF8.fromString moduleName)


cfMakeSymbol :: CustardContext
             -> [CustardValue]
             -> FruitTart (CustardContext, CustardValue)
cfMakeSymbol context parameters = do
  requireNParameters parameters 2 "makeSymbol"
  moduleName <- valueToUTF8String $ parameters !! 0
  properName <- valueToUTF8String $ parameters !! 1
  return (context,
          CustardSymbol (UTF8.toString moduleName) (UTF8.toString properName))

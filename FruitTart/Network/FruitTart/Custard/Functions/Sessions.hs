module Network.FruitTart.Custard.Functions.Sessions (
                                                     cfGetSessionID
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


cfGetSessionID :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfGetSessionID context parameters = do
  requireControllerContext context "getSessionID"
  requireNParameters parameters 0 "getSessionID"
  FruitTartState { sessionID = sessionID } <- get
  return $ CustardInteger $ fromJust sessionID

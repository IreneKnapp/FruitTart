module Network.FruitTart.Custard.Functions.Passwords (
                                                      cfHashPassword
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


cfHashPassword :: CustardContext
               -> [CustardValue]
               -> FruitTart CustardValue
cfHashPassword context parameters = do
  requireNParameters parameters 1 "hashPassword"
  error "Not yet implemented."
  -- TODO

module Network.FruitTart.Custard.Semantics (
					    getTemplateWithParameters,
					    eval,
                                            applyFunctionGivenName,
                                            applyFunctionGivenContextAndValue,
					    builtinBindings
					   )
    where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (catch)

import Network.FruitTart.Types


getTemplateWithParameters :: String
                          -> String
			  -> CustardContextType
			  -> Map String String
                          -> [CustardValue]
                          -> FruitTart String


eval :: String -> String -> FruitTart CustardValue


applyFunctionGivenName :: CustardContextType
                       -> Map String String
                       -> String
                       -> String
                       -> [CustardValue]
                       -> FruitTart (CustardContext, CustardValue)


applyFunctionGivenContextAndValue :: CustardContext
                                  -> CustardValue
                                  -> [CustardValue]
                                  -> FruitTart (CustardContext, CustardValue)


builtinBindings :: Map (String, String) CustardValue

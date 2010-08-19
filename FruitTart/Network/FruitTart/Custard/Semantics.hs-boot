module Network.FruitTart.Custard.Semantics (
					    getTemplateWithParameters,
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
                          -> [AnyCustardValue]
                          -> FruitTart String


applyFunctionGivenName :: CustardContextType
                       -> Map String String
                       -> String
                       -> String
                       -> [AnyCustardValue]
                       -> FruitTart (CustardContext, AnyCustardValue)


applyFunctionGivenContextAndValue :: CustardContext
                                  -> AnyCustardValue
                                  -> [AnyCustardValue]
                                  -> FruitTart (CustardContext, AnyCustardValue)


builtinBindings :: Map (String, String) AnyCustardValue

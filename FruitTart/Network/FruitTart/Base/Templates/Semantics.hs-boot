module Network.FruitTart.Base.Templates.Semantics (
       						   getTemplate,
                                                   fillTemplate,
						   eval,
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

import Network.FruitTart.Base
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


getTemplate :: String -> String -> [TemplateValue a] -> FruitTart String

fillTemplate :: String
             -> String
             -> FruitTart String

eval :: String -> String -> FruitTart (TemplateValue a)

builtinBindings :: Map (String, String) (TemplateValue a)

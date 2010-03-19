module Network.FruitTart.Base.Templates.Semantics (
                                                   fillTemplate,
						   eval,
                                                   baseBindings
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
import Network.FruitTart.Base.Templates.Syntax
import Network.FruitTart.Base.Templates.Types
import Network.FruitTart.Util


fillTemplate :: String
             -> String
             -> FruitTart String

eval :: String -> String -> FruitTart TemplateValue

baseBindings :: Map (String, String) TemplateValue

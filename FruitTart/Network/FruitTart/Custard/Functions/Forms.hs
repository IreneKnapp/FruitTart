module Network.FruitTart.Custard.Functions.Forms (
                                                  cfFormInput
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


cfFormInput :: CustardContext
            -> [CustardValue]
            -> FruitTart CustardValue
cfFormInput context parameters = do
  requireControllerContext context "formInput"
  requireNParameters parameters 1 "formInput"
  name <- valueToUTF8String $ head parameters
  let CustardContext { custardContextFormInputMap = formInputMap } = context
  return $ CustardMaybe
         $ case Map.lookup (UTF8.toString name) formInputMap of
             Nothing -> Nothing
             Just value -> Just $ CustardString $ UTF8.fromString value

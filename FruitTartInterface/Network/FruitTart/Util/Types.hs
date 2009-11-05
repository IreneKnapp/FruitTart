{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Network.FruitTart.Util.Types where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString
import Data.Int
import Data.Map (Map)
import Data.Dynamic
import Network.CGI
import Network.CGI.Monad

import qualified Database.SQLite3 as SQL

data FruitTartState  = FruitTartState {
      database :: SQL.Database,
      sessionID :: Maybe Int64,
      captchaCacheMVar :: MVar (Map Int64 (String, ByteString))
    }
instance Typeable (FruitTart CGIResult)
    where typeOf function = mkTyConApp (mkTyCon "FruitTart CGIResult") []


type FruitTart = StateT FruitTartState (CGIT IO)
liftCGI :: CGIT IO a -> FruitTart a
liftCGI = lift
instance MonadCGI FruitTart where
    cgiAddHeader name value = liftCGI $ cgiAddHeader name value
    cgiGet function = liftCGI $ cgiGet function

type ActionTable = Map String
                       (Map String
                            ([ParameterType],
                             [(String, ParameterType)],
                             Dynamic))
type ControllerTable = Map String ActionTable


data ParameterType = IDParameter
                   | StringParameter
                   | EitherStringIDParameter
                     deriving (Eq)

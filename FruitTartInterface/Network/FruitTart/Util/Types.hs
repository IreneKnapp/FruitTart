{-# LANGUAGE TypeSynonymInstances #-}
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
import Network.FruitTart.PluginInterface


data FruitTartState  = FruitTartState {
      database :: SQL.Database,
      interfacesMapMVar :: MVar (Map String Interface),
      interfaceStateMVarMap :: Map String Dynamic,
      sessionID :: Maybe Int64
    }

type FruitTart = StateT FruitTartState (CGIT IO)
instance Typeable1 FruitTart where
    typeOf1 function = mkTyConApp (mkTyCon "FruitTart") []
instance Typeable a => Typeable (FruitTart a) where
    typeOf = typeOfDefault

liftCGI :: CGIT IO a -> FruitTart a
liftCGI = lift
instance MonadCGI FruitTart where
    cgiAddHeader name value = liftCGI $ cgiAddHeader name value
    cgiGet function = liftCGI $ cgiGet function

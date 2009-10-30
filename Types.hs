{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString
import Data.Int
import Data.Map (Map)
import Data.Dynamic
import Network.CGI.Monad

import qualified SQLite3 as SQL

data BuglistState  = BuglistState {
      database :: SQL.Database,
      captchaCacheMVar :: MVar (Map Int64 (String, ByteString))
    }
type Buglist = StateT BuglistState (CGIT IO)
liftCGI :: CGIT IO a -> Buglist a
liftCGI = lift
instance MonadCGI Buglist where
    cgiAddHeader name value = liftCGI $ cgiAddHeader name value
    cgiGet function = liftCGI $ cgiGet function

type DispatchTable = Map String
                         (Map String
                              (Map String
                                   ([ParameterType],
                                    [(String, ParameterType)],
                                    Dynamic)))

data ParameterType = IDParameter
                   | OptionalStringParameter
                   | OptionalIDParameter
                     deriving (Eq)

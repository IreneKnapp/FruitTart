{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import Control.Monad.State
import Network.CGI.Monad

import qualified SQLite3 as SQL

data BuglistState  = BuglistState {
      database :: SQL.Database
    }
type Buglist = StateT BuglistState (CGIT IO)
liftCGI :: CGIT IO a -> Buglist a
liftCGI = lift
instance MonadCGI Buglist where
    cgiAddHeader name value = liftCGI $ cgiAddHeader name value
    cgiGet function = liftCGI $ cgiGet function

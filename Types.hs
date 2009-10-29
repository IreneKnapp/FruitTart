{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import Control.Monad.State
import Network.CGI.Monad

import qualified SQLite3 as SQL

data BuglistState  = BuglistState {
      database :: SQL.Database
    }
type Buglist = StateT BuglistState (CGIT IO)
instance MonadCGI Buglist where
    cgiAddHeader name value = lift $ cgiAddHeader name value
    cgiGet function = lift $ cgiGet function

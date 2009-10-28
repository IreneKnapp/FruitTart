module Buglist where

import qualified Dispatcher
import qualified SQLite3

main :: IO ()
main = do
  Dispatcher.main

module Controller.Tickets where

import {-# SOURCE #-} Dispatcher

index :: [String] -> [(String, String)] -> CGI CGIResult
index urlParameters namedParameters = do
  output $ "Nothing to see."


view :: [String] -> [(String, String)] -> CGI CGIResult
view urlParameters namedParameters = do
  output $ "Nothing to see."

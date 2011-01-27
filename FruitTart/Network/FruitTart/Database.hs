module Network.FruitTart.Database (earlyQuery,
                                   query,
                                   preparedQuery)
  where

import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Numeric
import System.Locale

import Database.SQLite3
import Network.FruitTart.Types


earlyQuery :: Database -> String -> [SQLData] -> IO [[SQLData]]
earlyQuery database query bindings = do
  statement <- prepare database query
  bind statement bindings
  result <- query' statement
  finalize statement
  return result
  where query' statement = do
          stepResult <- step statement
          case stepResult of
            Row -> do
              row <- columns statement
              remainder <- query' statement
              return $ [row] ++ remainder
            Done -> return []


query :: String -> [SQLData] -> FruitTart [[SQLData]]
query text bindings = do
  FruitTartState { database = database } <- get
  statement <- liftIO $ prepare database text
  liftIO $ bind statement bindings
  result <- query' statement
  liftIO $ finalize statement
  return result
  where query' statement = do
          stepResult <- liftIO $ step statement
          case stepResult of
            Row -> do
                   row <- liftIO $ columns statement
                   remainder <- query' statement
                   return $ [row] ++ remainder
            Done -> return []


preparedQuery :: Statement -> [SQLData] -> FruitTart [[SQLData]]
preparedQuery statement bindings = do
  liftIO $ bind statement bindings
  result <- query' statement
  liftIO $ reset statement
  return result
  where query' statement = do
          stepResult <- liftIO $ step statement
          case stepResult of
            Row -> do
              row <- liftIO $ columns statement
              remainder <- query' statement
              return $ [row] ++ remainder
            Done -> return []

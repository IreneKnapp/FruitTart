module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.ByteString
import Data.Dynamic
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import System.Exit
import Database.SQLite3

import qualified Network.FruitTart.Captcha.Controller.Captcha
    as Controller.Captcha
import Network.FruitTart.Util


fruitTartPlugin :: Dynamic
fruitTartPlugin = toDyn $ Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("Base", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceInitRequest = initRequest
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("captcha", Controller.Captcha.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Captcha"


moduleVersion :: Int64
moduleVersion = 1


moduleSchemaVersion :: Int64
moduleSchemaVersion = 1


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES (?, ?)"
             [SQLText moduleName, SQLInteger moduleSchemaVersion]
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newMVar (Map.empty :: Map Int64 (String, ByteString))
  return $ toDyn mVar


initRequest :: FruitTart ()
initRequest = return ()

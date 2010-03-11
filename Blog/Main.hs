module Main (fruitTartPlugin) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import System.Environment
import System.Exit
import Database.SQLite3

import qualified Network.FruitTart.Adventure.Controller.Adventure
    as Controller.Adventure
import Network.FruitTart.Util


fruitTartPlugin :: Interface
fruitTartPlugin = Interface {
                    interfaceVersion = 1,
                    interfaceDispatchTable = dispatchTable,
                    interfaceModuleName = moduleName,
                    interfaceModuleVersion = moduleVersion,
                    interfaceModuleSchemaVersion = moduleSchemaVersion,
                    interfacePrerequisites = [("FruitTart", 1),
                                              ("Base", 1)],
                    interfaceInitDatabase = initDatabase,
                    interfaceInitState = initState,
                    interfaceInitRequest = initRequest
                  }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("blog", Controller.Blog.actionTable)]


fruitTartSchemaVersion :: Int64
fruitTartSchemaVersion = 1


moduleName :: String
moduleName = "Blog"


moduleVersion :: Int64
moduleVersion = 1


moduleSchemaVersion :: Int64
moduleSchemaVersion = 1


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES (?, ?)"
             [SQLText moduleName, SQLInteger moduleSchemaVersion]
  earlyQuery database
                 (  "CREATE TABLE blog_posts (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "author INTEGER,\n"
                 ++ "title TEXT,\n"
                 ++ "published INTEGER,\n"
                 ++ "allow_comments INTEGER,\n"
                 ++ "allow_pings INTEGER,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "timezone TEXT,\n"
                 ++ "body TEXT,\n"
                 ++ "extended_body TEXT,\n"
                 ++ "excerpt TEXT\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE blog_comments (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "post INTEGER,\n"
                 ++ "author INTEGER,\n"
                 ++ "ip TEXT,\n"
                 ++ "timestamp INTEGER,\n"
                 ++ "body TEXT\n"
                 ++ ")")
                 []
  return ()


initState :: IO Dynamic
initState = do
  mVar <- newEmptyMVar :: IO (MVar String)
  return $ toDyn mVar


initRequest :: FruitTart ()
initRequest = return ()

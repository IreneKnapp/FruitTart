module Main (main) where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.FastCGI (runFastCGIorCGI)
import Network.CGI.Monad
import System.Environment
import System.Exit
import System.IO.Unsafe
import System.Plugins

import Database.SQLite3
import Network.FruitTart.PluginInterface as PluginInterface
import Network.FruitTart.Util
import qualified Network.FruitTart.Controller.Login as Controller.Login
import qualified Network.FruitTart.Dispatcher as Dispatcher
import qualified Network.FruitTart.View.Login as View.Login
import qualified Network.FruitTart.View.Misc as View.Misc
import qualified Network.FruitTart.View.Navigation as View.Navigation
import qualified Network.FruitTart.View.PopupMessage as View.PopupMessage
import qualified Network.FruitTart.View.Users as View.Users


main :: IO ()
main = do
  databasePath <- getEnv "FRUITTART_DB"
  database <- open databasePath
  interfacesMVar <- newMVar $ Map.fromList [("FruitTart", baseInterface)]
  maybeInitDatabase database "FruitTart" schemaVersion Main.initDatabase
  loadInstalledModules database
  captchaCacheMVar <- newMVar $ Map.empty
  state <- return $ FruitTartState {
             database = database,
             interfacesMVar = interfacesMVar,
             sessionID = Nothing,
             captchaCacheMVar = captchaCacheMVar
           }
  runFastCGIorCGI $ evalStateT (Dispatcher.processRequest Main.dispatchTable) state
  return ()


loadInstalledModules :: Database -> IO ()
loadInstalledModules database = do
  initLinker
  rows <- earlyQuery database "SELECT name FROM installed_modules" []
  interfaces
      <- mapM (\[SQLText name] -> do
                maybeInterface <- loadPackageFunction name "Main" "fruitTartPlugin"
                case maybeInterface of
                  Nothing -> do
                    logCGI $ "Not installed or not a plugin: " ++ name
                    return []
                  Just interface -> return [interface])
              rows
         >>= return . concat
  let allModules = map (\interface -> (interfaceModuleName interface,
                                       interfaceModuleVersion interface))
                       interfaces
      dependencyMap
         = Map.fromList
         $ map (\interface ->
                  let name = interfaceModuleName interface
                      version = interfaceModuleVersion interface
                      prerequisites = interfacePrerequisites interface
                  in ((name, version), prerequisites))
              interfaces
      findLoadOrder unprocessedDependencyMap
          = let importantModules = findLoadOrder' unprocessedDependencyMap
                unimportantModules = allModules \\ importantModules
            in concat [importantModules, unimportantModules]
      findLoadOrder' unprocessedDependencyMap
          = case Map.keys unprocessedDependencyMap of
              [] -> []
              (arbitraryModule:_)
                  -> let rootModule = findRootModule arbitraryModule
                         findRootModule someModule = case parentModule someModule of
                                                   Nothing -> someModule
                                                   Just anotherModule
                                                       -> findRootModule anotherModule
                         parentModule someModule
                             = case Map.lookup someModule unprocessedDependencyMap
                                 of Nothing -> Nothing
                                    Just parentModules -> Just $ head parentModules
                         removeFromDependencyMap moduleToRemove dependencyMap
                             = let mapWithoutModuleAsParent moduleToRemove someMap
                                       = Map.map (\modules -> delete moduleToRemove
                                                                     modules)
                                                 someMap
                                   mapWithoutParentlessModules someMap
                                       = Map.foldWithKey
                                         (\someModule dependencies mapBeingProcessed
                                            -> if dependencies == []
                                                 then Map.delete someModule
                                                                 mapBeingProcessed
                                                 else mapBeingProcessed)
                                         someMap
                                         someMap
                               in mapWithoutParentlessModules
                                  $ mapWithoutModuleAsParent moduleToRemove dependencyMap
                     in rootModule
                        : (findLoadOrder'
                           $ removeFromDependencyMap rootModule unprocessedDependencyMap)
      loadOrder = findLoadOrder dependencyMap
  putStrLn $ show $ map interfaceModuleName interfaces
  putStrLn $ show dependencyMap
  putStrLn $ show loadOrder
  return ()


importFunctionTableMVar :: MVar CombinedFunctionTable
importFunctionTableMVar = unsafePerformIO newEmptyMVar


baseInterface :: Interface
baseInterface = Interface {
                  interfaceVersion = 1,
                  interfaceDispatchTable = dispatchTable,
                  interfaceFunctionTable = functionTable,
                  interfaceModuleName = "FruitTart",
                  interfaceModuleVersion = moduleVersion,
                  interfaceModuleSchemaVersion = schemaVersion,
                  interfacePrerequisites = [],
                  interfaceInitDatabase = initDatabase,
                  interfaceImportFunctionTableMVar = importFunctionTableMVar
                }


dispatchTable :: ControllerTable
dispatchTable
    = combineActionTables
      [("login", Controller.Login.actionTable)]


functionTable :: CombinedFunctionTable
functionTable
    = combineFunctionTables
      [("Controller.Login", Controller.Login.functionTable),
       ("View.Login", View.Login.functionTable),
       ("View.Misc", View.Misc.functionTable),
       ("View.Navigation", View.Navigation.functionTable),
       ("View.PopupMessage", View.PopupMessage.functionTable),
       ("View.Users", View.Users.functionTable)]


moduleVersion :: Int64
moduleVersion = 1


schemaVersion :: Int64
schemaVersion = 1


maybeInitDatabase :: Database -> String -> Int64 -> (Database -> IO ()) -> IO ()
maybeInitDatabase database name requiredVersion initDatabase = do
  [[SQLInteger count]]
      <- earlyQuery database
                    "SELECT count(*) FROM schema_versions WHERE module = ?"
                    [SQLText name]
  case count of
    0 -> do
      initDatabase database
      return ()
    1 -> do
      [[SQLInteger presentVersion]]
          <- earlyQuery database
                        "SELECT version FROM schema_versions WHERE module = ?"
                        [SQLText name]
      if presentVersion /= requiredVersion
        then do
          logCGI $ "Schema mismatch for module " ++ name ++ ": Program version "
                 ++ (show requiredVersion) ++ ", database version "
                 ++ (show presentVersion) ++ "."
          exitFailure
        else return ()


initDatabase :: Database -> IO ()
initDatabase database = do
  earlyQuery database
                 (  "CREATE TABLE schema_versions (\n"
                 ++ "module TEXT PRIMARY KEY,\n"
                 ++ "version INTEGER"
                 ++ ")")
                 []
  earlyQuery database
             "INSERT INTO schema_versions (module, version) VALUES ('FruitTart', ?)"
             [SQLInteger schemaVersion]
  earlyQuery database
                 (  "CREATE TABLE installed_modules (\n"
                 ++ "name TEXT PRIMARY KEY\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE sessions (\n"
                 ++ "id INTEGER PRIMARY KEY,\n"
                 ++ "timestamp_activity INTEGER,\n"
                 ++ "recent_user INTEGER,\n"
                 ++ "logged_in_user INTEGER,\n"
                 ++ "popup_message TEXT\n"
                 ++ ")")
                 []
  earlyQuery database
                 (  "CREATE TABLE settings (\n"
                 ++ "owner_user INTEGER,"
                 ++ "anonymous_user INTEGER,"
                 ++ "nobody_user INTEGER,"
                 ++ "default_page TEXT"
                 ++ ")")
                 []
  earlyQuery database
                 (  "INSERT INTO settings "
                 ++ "(owner_user, anonymous_user, nobody_user, default_page) "
                 ++ "VALUES (NULL, NULL, NULL, '/login/account/')")
                 []
  earlyQuery database
                 (  "CREATE TABLE users (\n"
                 ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,\n"
                 ++ "full_name TEXT,\n"
                 ++ "email TEXT,\n"
                 ++ "password_hash BLOB\n"
                 ++ ")")
                 []
  earlyQuery database
             (  "INSERT INTO users (id, full_name, email, password_hash) "
             ++ "VALUES (1, 'Dan Knapp', 'dankna@gmail.com', ?, "
             ++ "1, 1, 1, 1, 1, 1, 1)")
             [SQLBlob $ hashPassword "This password must be changed."]
  earlyQuery database
            (  "INSERT INTO users (id, full_name, email, password_hash, "
            ++ "right_synchronize, right_admin_users, right_see_emails, "
            ++ "right_report_issues, right_modify_issues, right_upload_files, "
            ++ "right_comment_issues) "
            ++ "VALUES (2, 'Nobody', 'nobody', NULL, 0, 0, 0, 0, 0, 0, 0)")
            []
  earlyQuery database
            (  "INSERT INTO users (id, full_name, email, password_hash, "
            ++ "right_synchronize, right_admin_users, right_see_emails, "
            ++ "right_report_issues, right_modify_issues, right_upload_files, "
            ++ "right_comment_issues) "
            ++ "VALUES (3, 'Anonymous', 'anonymous', NULL, 0, 0, 0, 1, 0, 0, 1)")
            []
  earlyQuery database
            (  "UPDATE settings "
            ++ "SET owner_users = 1, anonymous_user = 3, nobody_user = 2")
            []
  return ()

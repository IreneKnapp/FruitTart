module Network.FruitTart.Templates.Imports (
                                            importFunctionTableMVar,
                                            getLoggedInUserID,
                                            getEffectiveUserID,
                                            outputMustLoginPage,
                                            getLoginButton,
                                            getPageHeadItems,
                                            defaultFullName,
                                            defaultEmail,
                                            privacyNote,
                                            getDefaultPage,
                                            getNavigationBar,
                                            getPopupMessage,
                                            setPopupMessage,
                                            getOrCreateUserID,
                                            getCanActAsUser
                                           )
    where

import Control.Concurrent.MVar
import Data.Int
import System.IO.Unsafe

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util


importFunctionTableMVar :: MVar CombinedFunctionTable
importFunctionTableMVar = unsafePerformIO newEmptyMVar


getLoggedInUserID :: FruitTart (Maybe Int64)
getLoggedInUserID
    = importFunction importFunctionTableMVar "Controller.Login" "getLoggedInUserID"

getEffectiveUserID :: FruitTart Int64
getEffectiveUserID
    = importFunction importFunctionTableMVar "Controller.Login" "getEffectiveUserID"

outputMustLoginPage :: String -> FruitTart CGIResult
outputMustLoginPage
    = importFunction importFunctionTableMVar "Controller.Login" "outputMustLoginPage"

getLoginButton :: String -> FruitTart String
getLoginButton
    = importFunction importFunctionTableMVar "View.Login" "getLoginButton"

getPageHeadItems :: FruitTart String
getPageHeadItems
    = importFunction importFunctionTableMVar "View.Misc" "getPageHeadItems"

defaultFullName :: String
defaultFullName
    = importFunction importFunctionTableMVar "View.Misc" "defaultFullName"

defaultEmail :: String
defaultEmail
    = importFunction importFunctionTableMVar "View.Misc" "defaultEmail"

privacyNote :: String
privacyNote
    = importFunction importFunctionTableMVar "View.Misc" "privacyNote"

getDefaultPage :: FruitTart String
getDefaultPage
    = importFunction importFunctionTableMVar "View.Navigation" "getDefaultPage"

getNavigationBar :: String -> FruitTart String
getNavigationBar
    = importFunction importFunctionTableMVar "View.Navigation" "getNavigationBar"

getPopupMessage :: FruitTart String
getPopupMessage
    = importFunction importFunctionTableMVar "View.PopupMessage" "getPopupMessage"

setPopupMessage :: Maybe String -> FruitTart ()
setPopupMessage
    = importFunction importFunctionTableMVar "View.PopupMessage" "setPopupMessage"

getOrCreateUserID :: String -> String -> FruitTart Int64
getOrCreateUserID
    = importFunction importFunctionTableMVar "View.Users" "getOrCreateUserID"

getCanActAsUser :: Int64 -> FruitTart Bool
getCanActAsUser
    = importFunction importFunctionTableMVar "View.Users" "getCanActAsUser"

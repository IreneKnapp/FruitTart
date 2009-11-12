module Network.FruitTart.Buglist.Imports (
                                          importFunctionTableMVar,
                                          generateCaptcha,
                                          checkCaptcha,
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
                                          getCanActAsUser,
                                          TemplateValueType(..),
                                          bindBool,
                                          bindInt,
                                          bindString,
                                          bindStringList,
                                          bindQuery,
                                          bindQueryMultipleRows,
                                          getTemplate
                                         )
    where

import Control.Concurrent.MVar
import Data.Int
import System.IO.Unsafe

import Network.FruitTart.PluginInterface
import Network.FruitTart.Util
import Network.FruitTart.Templates.Types


importFunctionTableMVar :: MVar CombinedFunctionTable
importFunctionTableMVar = unsafePerformIO newEmptyMVar


generateCaptcha :: FruitTart Int64
generateCaptcha
    = importFunction importFunctionTableMVar
      "Captcha.Controller.Captcha" "generateCaptcha"

checkCaptcha :: Int64 -> String -> FruitTart Bool
checkCaptcha
    = importFunction importFunctionTableMVar
      "Captcha.Controller.Captcha" "checkCaptcha"

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

bindBool :: String -> String -> Bool -> FruitTart ()
bindBool
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindBool"

bindInt :: String -> String -> Int64 -> FruitTart ()
bindInt
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindInt"

bindString :: String -> String -> String -> FruitTart ()
bindString
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindString"

bindStringList :: String -> String -> [String] -> FruitTart ()
bindStringList
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindStringList"

bindQuery :: String -> [(String, TemplateValueType)]
          -> String -> [SQLData]
          -> FruitTart ()
bindQuery
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindQuery"

bindQueryMultipleRows :: String -> String -> [(String, TemplateValueType)]
                      -> String -> [SQLData]
                      -> FruitTart ()
bindQueryMultipleRows
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "bindQueryMultipleRows"

getTemplate :: String -> String -> FruitTart String
getTemplate
    = importFunction importFunctionTableMVar
      "Templates.Controller.Templates" "getTemplate"

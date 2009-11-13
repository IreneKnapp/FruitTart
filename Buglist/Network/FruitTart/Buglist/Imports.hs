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
                                          TemplateValue(..),
                                          bind,
                                          bindQuery,
                                          bindQueryMultipleRows,
                                          convertRowToBindings,
                                          getTemplate
                                         )
    where

import Control.Concurrent.MVar
import Data.Int
import Data.Map (Map)
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

getOrCreateUserID :: String -> String -> FruitTart Int64
getOrCreateUserID
    = importFunction importFunctionTableMVar "View.Users" "getOrCreateUserID"

getCanActAsUser :: Int64 -> FruitTart Bool
getCanActAsUser
    = importFunction importFunctionTableMVar "View.Users" "getCanActAsUser"

bind :: (Bindable a) => String -> String -> a -> FruitTart ()
bind moduleName templateName bindable
    = (importFunction importFunctionTableMVar
       "Templates.View.Templates" "bind")
      moduleName templateName $ AnyBindable bindable

bindQuery :: String -> [(String, TemplateValueType)]
          -> String -> [SQLData]
          -> FruitTart ()
bindQuery
    = importFunction importFunctionTableMVar
      "Templates.View.Templates" "bindQuery"

bindQueryMultipleRows :: String -> String -> [(String, TemplateValueType)]
                      -> String -> [SQLData]
                      -> FruitTart ()
bindQueryMultipleRows
    = importFunction importFunctionTableMVar
      "Templates.View.Templates" "bindQueryMultipleRows"

convertRowToBindings :: String -> [(String, TemplateValueType)] -> [SQLData]
                     -> Map (String, String) TemplateValue
convertRowToBindings
    = importFunction importFunctionTableMVar
      "Templates.View.Templates" "convertRowToBindings"

unbind :: String -> String -> FruitTart ()
unbind
    = importFunction importFunctionTableMVar
      "Templates.View.Templates" "unbind"

getTemplate :: String -> String -> FruitTart String
getTemplate
    = importFunction importFunctionTableMVar
      "Templates.View.Templates" "getTemplate"

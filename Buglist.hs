module Buglist (
                getUser,
                getLoggedInUser,
                getEffectiveUser,
                getCanActAsUser,
                getRightSynchronize,
                getRightAdminUsers,
                getRightSeeEmails,
                getRightReportIssues,
                getRightModifyIssues,
                getRightUploadFiles,
                getRightCommentIssues,
                getPageHeadItems,
                getNavigationBar,
                getLoginButton,
                setPopupMessage,
                getPopupMessage,
                getSubnavigationBar,
                getStatusPopup,
                getResolutionPopup,
                getModulePopup,
                getSeverityPopup,
                getPriorityPopup,
                privacyNote,
                defaultFullName,
                defaultEmail
               )
    where

import Control.Concurrent
import Control.Monad.State
import Data.Dynamic
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Network.FastCGI hiding (logCGI)
import Network.CGI.Monad
import System.Environment
import System.Exit

import qualified Database.SQLite3 as SQL
import Network.FruitTart.Dispatcher
import Network.FruitTart.Util









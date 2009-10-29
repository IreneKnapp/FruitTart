module Log where

import Control.Monad.Trans
import Prelude hiding (log)
import System.IO


logCGI :: MonadIO m => String -> m ()
logCGI string = 
    liftIO $ withFile "/tmp/Buglist.error.log"
                      AppendMode
                      (\handle -> do
                         hPutStrLn handle string)

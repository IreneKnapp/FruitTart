module Network.FruitTart.Util.Log where

import Control.Monad.Trans
import Prelude hiding (log)
import System.Environment
import System.IO


logCGI :: MonadIO m => String -> m ()
logCGI string = do
  logfileName <- liftIO $ getEnv "FRUITTART_ERROR_LOG"
  liftIO $ withFile logfileName
                    AppendMode
                    (\handle -> do
                       hPutStrLn handle string)

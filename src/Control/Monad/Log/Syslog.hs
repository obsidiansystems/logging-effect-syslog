module Control.Monad.Log.Syslog where

import Control.Monad.IO.Class
import Control.Monad.Log
import Data.ByteString.Char8 as C8
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import System.IO
import Data.String
import qualified System.Posix.Syslog as Posix

-- | Log messages to a posix system log. The string argument is a tag that can
-- be used to identify log messages produced by this logger.
-- You can, for instance, run @journalctl --user -t mytag@ to see log messages
-- tagged with @"mytag"@.
logToSyslog :: (MonadIO m) => String -> Handler m (WithSeverity ByteString)
logToSyslog tagstr = \(WithSeverity sev msg) ->
  liftIO $ Posix.withSyslog tagstr [Posix.DelayedOpen] Posix.User $
    unsafeUseAsCStringLen msg $
      Posix.syslog Nothing (syslogPriority sev)

syslogPriority :: Severity -> Posix.Priority
syslogPriority = \case
  Emergency -> Posix.Emergency  
  Alert -> Posix.Alert  
  Critical -> Posix.Critical  
  Error -> Posix.Error  
  Warning -> Posix.Warning  
  Notice -> Posix.Notice  
  Informational -> Posix.Info 
  Debug-> Posix.Debug

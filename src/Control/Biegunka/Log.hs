-- | Biegunka logger
--
-- Provides an interface for consistent logging from multithreaded interpreters
module Control.Biegunka.Log
  ( -- * Types
    Logger, plain, exception
    -- * Acquire/release
  , start, stop
    -- * Operation
  , write
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue
import qualified System.IO as IO


-- | 'Logger' abstract data type for logging
newtype Logger = Logger (TQueue Command)

-- Different logger messages
--
-- this ADT is supposed to be more sophisticated
data Command =
    Display Message -- ^ Message to display
  | Stop (MVar ())  -- ^ Tells logger to stop

data Message =
    Plain { str :: String }     -- write to stdout
  | Exception { str :: String } -- write to stderr

-- | Plain log message about anything
plain :: String -> Message
plain = Plain

-- | Exception (or other error) log message
exception :: String -> Message
exception = Exception


-- | Get a new logger ready for logging
start :: IO Logger
start = do
  queue <- newTQueueIO
  forkIO (loop queue)
  return (Logger queue)
 where
  loop :: TQueue Command -> IO ()
  loop queue = do
    command <- atomically (readTQueue queue)
    case command of
      Display message -> do
        IO.hPutStr (logStream message) (str message)
        IO.hFlush IO.stdout
        loop queue
      Stop var  -> putMVar var ()

logStream :: Message -> IO.Handle
logStream (Plain _)     = IO.stdout
logStream (Exception _) = IO.stderr

-- | Stop logger
--
-- Blocks until logger confirms that it stops
stop :: Logger -> IO ()
stop (Logger queue) = do
  var <- newEmptyMVar
  atomically (writeTQueue queue (Stop var))
  takeMVar var


-- | Write a document
write :: Logger -> Message -> IO ()
write (Logger queue) message = atomically $
  writeTQueue queue (Display message)

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
import qualified System.Console.Terminal.Size as Term
import           System.IO (Handle, hFlush, stderr, stdout)
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayIO, renderPretty)


-- | 'Logger' abstract data type for logging
newtype Logger = Logger (TQueue Command)

-- Different logger messages
--
-- this ADT is supposed to be more sophisticated
data Command =
    Display Message -- ^ Message to display
  | Stop (MVar ())  -- ^ Tells logger to stop

data Message =
    Plain { doc :: Doc }     -- write to stdout
  | Exception { doc :: Doc } -- write to stderr

-- | Plain log message about anything
plain :: Doc -> Message
plain = Plain

-- | Exception (or other error) log message
exception :: Doc -> Message
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
        width <- fmap (maybe 80 Term.width) Term.size
        displayIO (logStream message) (renderPretty 0.9 width (doc message))
        hFlush stdout
        loop queue
      Stop var  -> putMVar var ()

logStream :: Message -> Handle
logStream (Plain _)     = stdout
logStream (Exception _) = stderr

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

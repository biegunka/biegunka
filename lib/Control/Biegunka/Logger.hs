-- | Biegunka logger
--
-- Provides an interface for consistent logging from multithreaded interpreters
module Control.Biegunka.Logger
  ( -- * Types
    Logger
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
import           System.IO (hFlush, stdout)
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayIO, renderPretty)


-- | 'Logger' abstract data type for logging
newtype Logger = Logger (TQueue Message)

-- Different logger messages
--
-- this ADT is supposed to be more sophisticated
data Message =
    Message Doc    -- ^ Message to display
  | Stop (MVar ()) -- ^ Tells logger to stop


-- | Get a new logger ready for logging
start :: IO Logger
start = do
  queue <- newTQueueIO
  forkIO (loop queue)
  return (Logger queue)
 where
  loop :: TQueue Message -> IO ()
  loop queue = do
    message <- atomically (readTQueue queue)
    case message of
      Message doc -> do
        width <- fmap (maybe 80 Term.width) Term.size
        displayIO stdout (renderPretty 0.9 width doc)
        hFlush stdout
        loop queue
      Stop var  -> putMVar var ()

-- | Stop logger
--
-- Blocks until logger confirms that it stops
stop :: Logger -> IO ()
stop (Logger queue) = do
  var <- newEmptyMVar
  atomically (writeTQueue queue (Stop var))
  takeMVar var


-- | Write a document
write :: Logger -> Doc -> IO ()
write (Logger queue) doc = atomically (writeTQueue queue (Message doc))

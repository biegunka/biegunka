{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- | A synchronized logger for multithreaded environments.
module Control.Biegunka.Logger
  ( Logger
  , HasLogger(logger)
  , with
  , write
  , writeSTM
    -- * Low-level operations
  , start
  , stop
  ) where

import           Control.Applicative (Const)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TQueue
import           Control.Lens (LensLike', forOf_, to)
import           Control.Lens (Traversed)
import           Control.Monad.Catch (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Function (fix)
import qualified System.IO as IO
import qualified System.IO.Error as IO


-- | 'Logger' is an abstract data type for logging.
newtype Logger = Logger { unLogger :: TQueue Command }

-- | Structures that have a 'Logger' inside of them.
class HasLogger c a | a -> c where
  logger :: c f => LensLike' f a Logger

instance HasLogger Functor Logger where
  logger = id

-- Logger messages.
data Command =
    Display IO.Handle String -- ^ Display a message
  | Stop (MVar ())           -- ^ Take a poison pill

-- | Run an action with a 'Logger' listening.
with :: (MonadIO m, MonadMask m) => (Logger -> m a) -> m a
with = bracket start stop

-- | Start a new logger.
start :: MonadIO m => m Logger
start = liftIO $ do
  q <- newTQueueIO
  forkIO (consume q)
  return (Logger q)
 where
  consume q = fix $ \loop ->
    atomically (readTQueue q) >>= \case
      Display h msg ->
        do IO.tryIOError (do IO.hPutStr h msg; IO.hFlush h)
           loop
      Stop var      -> putMVar var ()

-- | Stop the logger.
--
-- Blocks, waiting for the logger to process all queued messages.
stop :: MonadIO m => Logger -> m ()
stop (Logger queue) = liftIO $ do
  var <- newEmptyMVar
  atomically (writeTQueue queue (Stop var))
  takeMVar var

-- | Atomically write a string to a 'IO.Handle'.
write :: (HasLogger c l, c (Const (Traversed () STM))) => IO.Handle -> l -> String -> IO ()
write h l = atomically . writeSTM h l

-- | Schedule a write of a string to a 'IO.Handle' inside an 'STM' transaction.
writeSTM :: (HasLogger c l, c (Const (Traversed () STM))) => IO.Handle -> l -> String -> STM ()
writeSTM h l msg =
  forOf_ (logger.to unLogger)
         l
         (\l' -> writeTQueue l' (Display h msg))

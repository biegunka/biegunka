{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Execution tasks scheduler
module Biegunka.Execute.Schedule
  ( runTask, schedule
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (TQueue, readTQueue)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (evalStateT)
import           Data.Functor.Trans.Tagged (untag)
import           Data.Proxy (Proxy)
import           Data.Reflection (Reifies, reify)

import Biegunka.Control
import Biegunka.Execute.Control
import Biegunka.Language (Term(..))
import Biegunka.Script


-- | Prepares environment to run task with given execution routine
runTask :: forall s a. Settings Execution -- ^ Environment settings
        -> TaskLocal -- ^ Context
        -> (forall t. Reifies t (Settings Execution)
                => Free (Term Annotate s) a
                -> Executor t ()) -- ^ Task routine
        -> (Free (Term Annotate s) a) -- ^ Task contents
        -> IO ()
runTask e s f i =
  reify e ((`evalStateT` s) . untag . asProxyOf (f i))
{-# INLINE runTask #-}

-- | Thread `s' parameter to 'task' function
asProxyOf :: Executor s () -> Proxy s -> Executor s ()
asProxyOf a _ = a
{-# INLINE asProxyOf #-}


-- | Schedule tasks
--
-- "Forks" on every incoming workload
schedule :: TQueue Work -> IO ()
schedule j = go 0
 where
  go :: Int -> IO ()
  go n
    | n < 0     = return ()
    | otherwise = atomically (readTQueue j) >>= \t -> case t of
        Do w -> forkIO w >> go (n + 1)
        Stop -> go (n - 1)

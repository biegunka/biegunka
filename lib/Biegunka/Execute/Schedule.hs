{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Execution tasks scheduler
module Biegunka.Execute.Schedule
  ( runTask, schedule
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (TQueue, readTQueue)
import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (evalStateT)
import           Data.Functor.Trans.Tagged (untag)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Proxy (Proxy)
import           Data.Reflection (Reifies, reify)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Q

import Biegunka.Execute.Control
import Biegunka.Language (Term(..))
import Biegunka.Script


-- | Prepares environment to run task with given execution routine
runTask :: forall s a.
          EE STM -- ^ Environment
        -> EC -- ^ Context
        -> (forall t. Reifies t (EE STM) => Free (Term SA s) a -> Execution t ()) -- ^ Task routine
        -> (Free (Term SA s) a) -- ^ Task contents
        -> IO ()
runTask e s f i =
  reify e ((`evalStateT` s) . untag . asProxyOf (f i))
{-# INLINE runTask #-}

-- | Thread `s' parameter to 'task' function
asProxyOf :: Execution s () -> Proxy s -> Execution s ()
asProxyOf a _ = a
{-# INLINE asProxyOf #-}


-- | Schedule tasks
--
-- "Forks" on every incoming workload
schedule :: TQueue Work -> IO ()
schedule j = go 0 IM.empty IM.empty
 where
  go :: Int -> IntMap Int -> IntMap (Seq (IO ())) -> IO ()
  go n a b
    | n < 0     = return ()
    | otherwise = atomically (readTQueue j) >>= \t -> case t of
        Do i w -> do
          let n' = n + 1
              a' = a & at i . non 0 +~ 1
          case a^?ix i of
            Nothing -> forkIO w >> go n' a' b
            Just _  -> go n' a' (b & at i . anon Q.empty Q.null %~ (|> w))
        Stop i -> do
          let n' = n - 1
              a' = a & at i . non 0 -~ 1
          case b^?ix i.to uncons.traverse of
            Just (w, q) -> forkIO w >> go n' a' (b & at i . anon Q.empty Q.null .~ q)
            Nothing -> go n' a' b

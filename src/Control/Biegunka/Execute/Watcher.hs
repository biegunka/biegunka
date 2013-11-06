{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Execution 'Watcher'
module Control.Biegunka.Execute.Watcher
  ( -- * Watcher
    Watcher, new, wait, register, unregister, done, waitDone
  ) where

import           Control.Concurrent.STM (atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad (when, unless)
import           Control.Monad.Trans (MonadIO(..))
import           Data.Set (Set)
import qualified Data.Set as S

import           Control.Biegunka.Language (Token)

-- | 'Watcher' abstract data type
--
-- Watcher /watches/ number of jobs currently running
-- and subtasks ids that are already done execution
data Watcher = Watcher (TVar Int) (TVar (Set Token))

-- | Create 'new' 'Watcher'
--
-- Newly created 'Watcher' does not know about any jobs and tasks
new :: IO Watcher
new = do
  jobsvar <- newTVarIO 0
  donevar <- newTVarIO S.empty
  return (Watcher jobsvar donevar)

-- | Create 'new' 'Watcher'
--
-- Waits until job counter reaches @0@
--
-- @
-- 'new' >>= 'wait' ≡ return ()
-- @
wait :: Watcher -> IO ()
wait (Watcher var _) = atomically $ do
  jobs <- readTVar var
  when (jobs > 0)
    retry

-- | 'Watcher' jobs counter goes up
register :: MonadIO m => Watcher -> m ()
register (Watcher jobsvar _) = liftIO $ atomically (modifyTVar' jobsvar succ)

-- | 'Watcher' jobs counter goes down
unregister :: MonadIO m => Watcher -> m ()
unregister (Watcher jobsvar _) = liftIO . atomically $
  modifyTVar' jobsvar pred


-- | notify 'Watcher' subtask is done
done :: MonadIO m => Watcher -> Token -> m ()
done (Watcher _ donevar) tok = liftIO . atomically $
  modifyTVar' donevar (S.insert tok)

-- | Wait until all those subtasks are done
waitDone :: MonadIO m => Watcher -> Set Token -> m ()
waitDone (Watcher _ donevar) waits = liftIO . atomically $ do
  dones <- readTVar donevar
  unless (waits `S.isSubsetOf` dones)
    retry

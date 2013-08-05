{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling execution
module Control.Biegunka.Execute.Settings
  ( Executor, env
    -- * Executor environment
  , Execution
    -- * Lenses
  , work, running, sudoing, repos, tasks
    -- * Initializations
  , initializeSTM
    -- * Auxiliary types
  , Work(..)
  ) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens
import Data.Functor.Trans.Tagged
import Data.Monoid (mempty)
import Data.Reflection (Reifies)
import Data.Set (Set)


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor s a = TaggedT s IO a

-- | Get execution environment
env :: (Applicative m, Reifies s a) => TaggedT s m a
env = reflected


-- | Multithread accessable parts
data Execution = Execution
  { _work    :: TQueue Work       -- ^ Task queue
  , _sudoing :: TVar Bool         -- ^ Whether sudoed operation is in progress.
  , _running :: TVar Bool         -- ^ Whether any operation is in progress.
  , _repos   :: TVar (Set String) -- ^ Already updated repositories
  , _tasks   :: TVar (Set Int)    -- ^ Done tasks
  }

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come
  | Stop       -- ^ Task is done


-- * Lenses

makeLensesWith (defaultRules & generateSignatures .~ False) ''Execution

-- | Executor cross-thread state

-- | Task queue
work :: Lens' Execution (TQueue Work)

-- | Whether sudoed operation is in progress.
sudoing :: Lens' Execution (TVar Bool)

-- | Whether any operation is in progress.
running :: Lens' Execution (TVar Bool)

-- | Already updated repositories
repos :: Lens' Execution (TVar (Set String))

-- | Done tasks
tasks :: Lens' Execution (TVar (Set Int))


-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: IO Execution
initializeSTM = do
  a <- newTQueueIO
  b <- newTVarIO False
  c <- newTVarIO False
  d <- newTVarIO mempty
  e <- newTVarIO mempty
  return $ Execution
    { _work = a
    , _running = b
    , _sudoing = c
    , _repos = d
    , _tasks = e
    }

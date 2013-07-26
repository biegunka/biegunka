{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling execution
module Control.Biegunka.Execute.Settings
  ( Executor, env
    -- * Executor task-local state control
  , TaskLocal, reactStack, usersStack, retryCount
    -- * Executor environment
  , Execution, Sync, Run
    -- * Lenses
  , sync, runs
  , work, running, sudoing, repos, tasks
  , react, templates, retries, mode
    -- * Initializations
  , initializeSTM
    -- * Auxiliary types
  , Work(..), Templates(..), Mode(..)
  ) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens
import Control.Monad.State (StateT)
import Data.Default
import Data.Functor.Trans.Tagged
import Data.Monoid (mempty)
import Data.Reflection (Reifies)
import Data.Set (Set)
import Text.StringTemplate (ToSElem(..))

import Control.Biegunka.Language (React(..), User(..))


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor s a = TaggedT s (StateT TaskLocal IO) a

env :: (Applicative m, Reifies s a) => TaggedT s m a
env = reflected


-- | 'Executor' task-local state. Contains:
--
--   * Active failure reactions stack.
--
--   * Effective users names stack.
--
--   * Retry count for current task.
data TaskLocal = TaskLocal
  { _reactStack :: [React] -- ^ Saved reactions modificators. Topmost is active
  , _usersStack :: [User]  -- ^ Saved user chaning modificators. Topmost is active
  , _retryCount :: Int     -- ^ Performed retries for task
  } deriving (Show, Read)

instance Default TaskLocal where
  def = TaskLocal
    { _reactStack = []
    , _usersStack = []
    , _retryCount = 0
    }

makeLensesWith (defaultRules & generateSignatures .~ False) ''TaskLocal

-- | Saved reactions modificators. Topmost is active
reactStack :: Lens' TaskLocal [React]

-- | Saved user chaning modificators. Topmost is active
usersStack :: Lens' TaskLocal [User]

-- | Performed retries for task
retryCount :: Lens' TaskLocal Int


-- | Both 'Executor' environment and synced multithread state
data Execution = Execution
  { _sync :: Sync
  , _runs :: Run
  }

-- | Multithread accessable parts of 'Execution'
data Sync = Sync
  { _work    :: TQueue Work       -- ^ Task queue
  , _sudoing :: TVar Bool         -- ^ Whether sudoed operation is in progress.
  , _running :: TVar Bool         -- ^ Whether any operation is in progress.
  , _repos   :: TVar (Set String) -- ^ Already updated repositories
  , _tasks   :: TVar (Set Int)    -- ^ Done tasks
  }

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come and its id
  | Stop       -- ^ Task with that id is done

-- | 'Executor' environment.
-- Denotes default failure reaction, templates used and more
data Run = Run
  { _react     :: React       -- ^ How to react on failures
  , _templates :: Templates   -- ^ Templates mapping
  , _retries   :: Int         -- ^ Maximum retries count
  , _mode      :: Mode        -- ^ Executor mode
  }

instance Default Run where
  def = Run
    { _react     = def
    , _templates = Templates ()
    , _retries   = 1
    , _mode      = Dry
    }

-- | How to do execution
data Mode =
    Dry  -- ^ Dry run mode
  | Real -- ^ Real run mode
    deriving (Show, Read, Eq, Ord)

-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. ToSElem t => Templates t


-- * Lenses

makeLensesWith (defaultRules & generateSignatures .~ False) ''Execution

-- | Executor cross-thread state
sync :: Lens' Execution Sync

-- | Executor environment
runs :: Lens' Execution Run


makeLensesWith (defaultRules & generateSignatures .~ False) ''Sync

-- | Task queue
work :: Lens' Sync (TQueue Work)

-- | Whether sudoed operation is in progress.
sudoing :: Lens' Sync (TVar Bool)

-- | Whether any operation is in progress.
running :: Lens' Sync (TVar Bool)

-- | Already updated repositories
repos :: Lens' Sync (TVar (Set String))

-- | Done tasks
tasks :: Lens' Sync (TVar (Set Int))


makeLensesWith (defaultRules & generateSignatures .~ False) ''Run

-- | How to react on failures
react :: Lens' Run React

-- | Templates mapping
templates :: Lens' Run Templates

-- | Maximum retries count
retries :: Lens' Run Int

-- | Executor mode
mode :: Lens' Run Mode


-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: Run -> IO Execution
initializeSTM r = do
  a <- newTQueueIO
  b <- newTVarIO False
  c <- newTVarIO False
  d <- newTVarIO mempty
  e <- newTVarIO mempty
  return $ Execution
    { _sync = Sync
        { _work = a
        , _running = b
        , _sudoing = c
        , _repos = d
        , _tasks = e
        }
    , _runs = r
    }

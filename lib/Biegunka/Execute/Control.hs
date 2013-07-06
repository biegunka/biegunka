{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling execution
module Biegunka.Execute.Control
  ( Executor
    -- * Executor task-local state control
  , TaskLocal, reactStack, usersStack, retryCount
    -- * Executor environment
  , Run, Execution, Sync
  , priviledges, react, templates, retries
  , work, running, sudoing, repos, mode
  , initializeSTM
  , sync, execution
    -- * Misc
  , Templates(..), Priviledges(..), Work(..), Mode(..)
  ) where

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens
import Control.Monad.State (StateT)
import Data.Default
import Data.Functor.Trans.Tagged
import Data.Monoid (mempty)
import Data.Set (Set)
import Text.StringTemplate (ToSElem(..))

import Biegunka.Language (React(..))


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor s a = TaggedT s (StateT TaskLocal IO) a


-- | 'Executor' task-local state. Contains:
--
--   * Active failure reactions stack.
--
--   * Effective users names stack.
--
--   * Retry count for current task.
data TaskLocal = TaskLocal
  { _reactStack  :: [React]  -- ^ Saved reactions modificators. Topmost is active
  , _usersStack  :: [String] -- ^ Saved user chaning modificators. Topmost is active
  , _retryCount  :: Int      -- ^ Performed retries for task
  } deriving (Show, Read, Eq, Ord)

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
usersStack :: Lens' TaskLocal [String]

-- | Performed retries for task
retryCount :: Lens' TaskLocal Int

-- | Multithread accessable parts of 'Execution'
data Sync = Sync
  { _work    :: TQueue Work       -- ^ Task queue
  , _sudoing :: TVar Bool         -- ^ Whether sudoed operation is in progress.
  , _running :: TVar Bool         -- ^ Whether any operation is in progress.
  , _repos   :: TVar (Set String) -- ^ Already updated repositories
  }

-- | Workload
data Work =
    Do Int (IO ()) -- ^ Task to come and its id
  | Stop Int       -- ^ Task with that id is done

makeLensesWith (defaultRules & generateSignatures .~ False) ''Sync

-- | Task queue
work :: Lens' Sync (TQueue Work)

-- | Whether sudoed operation is in progress.
sudoing :: Lens' Sync (TVar Bool)

-- | Whether any operation is in progress.
running :: Lens' Sync (TVar Bool)

-- | Already updated repositories
repos :: Lens' Sync (TVar (Set String))

-- | 'Executor' environment.
-- Denotes default failure reaction, templates used and more
data Execution = Execution
  { _priviledges :: Priviledges -- ^ What to do with priviledges if ran in sudo
  , _react       :: React       -- ^ How to react on failures
  , _templates   :: Templates   -- ^ Templates mapping
  , _retries     :: Int         -- ^ Maximum retries count
  , _mode        :: Mode        -- ^ Executor mode
  }

-- | Priviledges control.
-- Controls how to behave if started with sudo
data Priviledges =
    Drop     -- ^ Drop priviledges
  | Preserve -- ^ Preserve priviledges
    deriving (Show, Read, Eq, Ord)

-- | How to do execution
data Mode =
    Dry  -- ^ Dry run mode
  | Real -- ^ Real run mode
    deriving (Show, Read, Eq, Ord)

-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. ToSElem t => Templates t

makeLensesWith (defaultRules & generateSignatures .~ False) ''Execution

-- | What to do with priviledges if ran in sudo
priviledges :: Lens' Execution Priviledges

-- | How to react on failures
react :: Lens' Execution React

-- | Templates mapping
templates :: Lens' Execution Templates

-- | Maximum retries count
retries :: Lens' Execution Int

-- | Executor mode
mode :: Lens' Execution Mode

instance Default Execution where
  def = Execution
    { _priviledges = Preserve
    , _react       = Ignorant
    , _templates   = Templates ()
    , _retries     = 1
    , _mode        = Dry
    }


data Run = Run
  { _sync      :: Sync
  , _execution :: Execution
  }

makeLensesWith (defaultRules & generateSignatures .~ False) ''Run

-- | Executor cross-thread state
sync :: Lens Run Run Sync Sync

-- | Executor environment
execution :: Lens Run Run Execution Execution


-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: Execution -> IO Run
initializeSTM e = do
  a <- newTQueueIO
  b <- newTVarIO False
  c <- newTVarIO False
  d <- newTVarIO mempty
  return $ Run
    { _sync = Sync
        { _work = a
        , _running = b
        , _sudoing = c
        , _repos = d
        }
    , _execution = e
    }

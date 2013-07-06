{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling execution
module Biegunka.Execute.Control
  ( Executor
    -- * Executor thread state control
  , EC(..), reactStack, usersStack, retryCount
    -- * Executor environment
  , EE(..), STM(..)
  , priviledges, react, templates, retries
  , stm, work, running, sudoing, repos, mode
  , initializeSTM
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


-- | Convenient type alias for thread-local-state-ful IO
-- tagged with multithreaded execution environment @s@
type Executor s a = TaggedT s (StateT EC IO) a


-- | 'Executor' thread state.
-- Denotes current failure reaction, effective user id and more
data EC = EC
  { _reactStack  :: [React]  -- ^ Saved reactions modificators. Topmost is active
  , _usersStack  :: [String] -- ^ Saved user chaning modificators. Topmost is active
  , _retryCount  :: Int      -- ^ Performed retries for task
  } deriving (Show, Read, Eq, Ord)

instance Default EC where
  def = EC
    { _reactStack = []
    , _usersStack = []
    , _retryCount = 0
    }

makeLensesWith (defaultRules & generateSignatures .~ False) ''EC

-- | Saved reactions modificators. Topmost is active
reactStack :: Lens' EC [React]

-- | Saved user chaning modificators. Topmost is active
usersStack :: Lens' EC [String]

-- | Performed retries for task
retryCount :: Lens' EC Int

-- | Concurrent parts of 'EE'
data STM = STM
  { _work    :: TQueue Work -- ^ Task queue
  , _sudoing :: TVar Bool -- ^ Whether sudoed operation is in progress.
  , _running :: TVar Bool -- ^ Whether any operation is in progress.
  , _repos   :: TVar (Set String) -- ^ Already updated repositories
  }

-- | Workload
data Work =
    Do Int (IO ()) -- ^ Task to come and its id
  | Stop Int       -- ^ Task with that id is done

makeLensesWith (defaultRules & generateSignatures .~ False) ''STM

-- | Task queue
work :: Lens' STM (TQueue Work)

-- | Whether sudoed operation is in progress.
sudoing :: Lens' STM (TVar Bool)

-- | Whether any operation is in progress.
running :: Lens' STM (TVar Bool)

-- | Already updated repositories
repos :: Lens' STM (TVar (Set String))

instance Default STM where
  def = STM
    { _work = undefined
    , _sudoing = undefined
    , _running = undefined
    , _repos = undefined
    }

-- | 'Executor' environment.
-- Denotes default failure reaction, templates used and more
data EE a = EE
  { _priviledges :: Priviledges -- ^ What to do with priviledges if ran in sudo
  , _react       :: React       -- ^ How to react on failures
  , _templates   :: Templates   -- ^ Templates mapping
  , _retries     :: Int         -- ^ Maximum retries count
  , _mode        :: Mode        -- ^ Executor mode
  , _stm         :: a           -- ^ Executor cross-thread state
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

makeLensesWith (defaultRules & generateSignatures .~ False) ''EE

-- | What to do with priviledges if ran in sudo
priviledges :: Lens' (EE a) Priviledges

-- | How to react on failures
react :: Lens' (EE a) React

-- | Templates mapping
templates :: Lens' (EE a) Templates

-- | Maximum retries count
retries :: Lens' (EE a) Int

-- | Executor mode
mode :: Lens' (EE a) Mode

-- | Executor cross-thread state
stm :: Lens (EE a) (EE b) a b

instance Default a => Default (EE a) where
  def = EE
    { _priviledges = Preserve
    , _react       = Ignorant
    , _templates   = Templates ()
    , _retries     = 1
    , _stm         = def
    , _mode        = Dry
    }

-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: EE () -> IO (EE STM)
initializeSTM e = do
  a <- newTQueueIO
  b <- newTVarIO False
  c <- newTVarIO False
  d <- newTVarIO mempty
  return $ e & stm .~ STM
    { _work = a
    , _running = b
    , _sudoing = c
    , _repos = d
    }

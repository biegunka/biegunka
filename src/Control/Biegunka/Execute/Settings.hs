{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling execution
module Control.Biegunka.Execute.Settings
  ( Executor, env
    -- * Executor environment
  , Execution
    -- * Lenses
  , execution, watch, user, repos
    -- * Initializations
  , initializeSTM
    -- * Auxiliary types
  , Work(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens
import           Data.Functor.Trans.Tagged (TaggedT, reflected)
import           Data.Meep (Meep)
import qualified Data.Meep as Meep
import           Data.Reflection (Reifies)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (lookup, null)
import qualified System.Posix as Posix

import           Control.Biegunka.Execute.Watcher (Watcher)
import           Control.Biegunka.Settings (Settings, local)


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor s a = TaggedT s IO a

-- | Get execution environment
env :: (Applicative m, Reifies s a) => TaggedT s m a
env = reflected

-- | Multithread accessable parts
data Execution = Execution
  { _watch :: Watcher
  , _user  :: TVar (Meep Posix.CUid Int) -- ^ Current user id and sessions counter
  , _repos :: TVar (Set String)          -- ^ Already updated repositories
  }

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come
  | Stop       -- ^ Task is done

-- * Lenses

makeClassy ''Execution

instance HasExecution (Settings Execution) where
  execution = local

-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: Watcher -> IO Execution
initializeSTM watcher = Execution watcher <$> newTVarIO Meep.empty <*> newTVarIO Set.empty

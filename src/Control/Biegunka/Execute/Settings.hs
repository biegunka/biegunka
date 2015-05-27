{-# LANGUAGE FlexibleInstances #-}
-- | Controlling execution
module Control.Biegunka.Execute.Settings
  ( Executor
  , runExecutor
  , forkExecutor
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
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.Meep (Meep)
import qualified Data.Meep as Meep
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (lookup, null)
import qualified System.Posix as Posix

import           Control.Biegunka.Execute.Watcher (Watcher)
import qualified Control.Biegunka.Execute.Watcher as Watcher
import           Control.Biegunka.Settings (Settings, local)


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor a = ReaderT (Settings Execution) IO a

runExecutor :: Settings Execution -> Executor a -> IO a
runExecutor = flip runReaderT

forkExecutor :: Executor a -> Executor ()
forkExecutor io = do
  e <- ask
  Watcher.register (view watch e)
  liftIO (forkFinally (runExecutor e io)
                      (\_ -> Watcher.unregister (view watch e)))
  return ()

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

class HasExecution t where
  execution :: Lens' t Execution

  watch :: Lens' t Watcher
  watch = execution . \f x -> f (_watch x) <&> \y -> x { _watch = y }

  user :: Lens' t (TVar (Meep Posix.CUid Int))
  user = execution . \f x -> f (_user x) <&> \y -> x { _user = y }

  repos :: Lens' t (TVar (Set String))
  repos = execution . \f x -> f (_repos x) <&> \y -> x { _repos = y }

instance HasExecution Execution where
  execution = id
  {-# INLINE execution #-}

instance HasExecution (Settings Execution) where
  execution = local
  {-# INLINE execution #-}

-- | Prepare 'Executor' environment to stm transactions
initializeSTM :: Watcher -> IO Execution
initializeSTM watcher = Execution watcher <$> newTVarIO Meep.empty <*> newTVarIO Set.empty

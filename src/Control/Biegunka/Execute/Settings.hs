{-# LANGUAGE MultiParamTypeClasses #-}
-- | Controlling execution
module Control.Biegunka.Execute.Settings
  ( Executor
  , runExecutor
  , forkExecutor
    -- * Executor environment
  , Execution
  , HasExecution(..)
    -- * Initialize 'Execution'
  , withExecution
    -- * Auxiliary types
  , Work(..)
  ) where

import           Control.Exception (bracket)
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Meep (Meep)
import qualified Data.Meep as Meep
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (lookup, null)
import qualified System.IO.Temp as IO
import qualified System.Posix as Posix

import           Control.Biegunka.Logger (HasLogger(logger))
import           Control.Biegunka.Execute.Watcher (Watcher)
import qualified Control.Biegunka.Execute.Watcher as Watcher
import           Control.Biegunka.Settings (HasSettings(settings), Settings)


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor a = ReaderT Execution IO a

runExecutor :: Execution -> Executor a -> IO a
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
  { _watch    :: Watcher
  , _user     :: TVar (Meep Posix.CUid Int) -- ^ Current user id and sessions counter
  , _repos    :: TVar (Set String)          -- ^ Already updated repositories
  , _activeSource
              :: TVar (Maybe (NonEmpty String))
  , _settings :: Settings
  , _onlyDiff :: Bool
  , _tempDir  :: FilePath
  }

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come
  | Stop       -- ^ Task is done

class HasExecution t where
  execution :: Lens' t Execution

  watch :: Lens' t Watcher
  watch = execution . \f x -> f (_watch x) <&> \y -> x { _watch = y }
  {-# INLINE watch #-}

  user :: Lens' t (TVar (Meep Posix.CUid Int))
  user = execution . \f x -> f (_user x) <&> \y -> x { _user = y }
  {-# INLINE user #-}

  repos :: Lens' t (TVar (Set String))
  repos = execution . \f x -> f (_repos x) <&> \y -> x { _repos = y }
  {-# INLINE repos #-}

  activeSource :: Lens' t (TVar (Maybe (NonEmpty String)))
  activeSource = execution . \f x -> f (_activeSource x) <&> \y -> x { _activeSource = y }
  {-# INLINE onlyDiff #-}

  onlyDiff :: Lens' t Bool
  onlyDiff = execution . \f x -> f (_onlyDiff x) <&> \y -> x { _onlyDiff = y }
  {-# INLINE activeSource #-}

  tempDir :: Lens' t FilePath
  tempDir = execution . \f x -> f (_tempDir x) <&> \y -> x { _tempDir = y }
  {-# INLINE tempDir #-}

instance HasExecution Execution where
  execution = id
  {-# INLINE execution #-}

instance HasSettings Execution where
  settings f x = f (_settings x) <&> \y -> x { _settings = y }
  {-# INLINE settings #-}

instance HasLogger Applicative Execution where
  logger = settings.logger
  {-# INLINE logger #-}

-- | Set up an 'Execution' to be used by 'Executor'.
withExecution :: Settings -> (Execution -> IO a) -> IO a
withExecution s f =
  IO.withSystemTempDirectory "biegunka" $ \dir ->
    bracket Watcher.new Watcher.wait $ \watcher -> do
      e <-
        Execution watcher <$> newTVarIO Meep.empty
                          <*> newTVarIO Set.empty
                          <*> newTVarIO Nothing
                          <*> pure s
                          <*> pure False
                          <*> pure dir
      f e

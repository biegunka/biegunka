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
    -- * Mip
  , Mip(..), singleton, fromList, lookup, insert, delete, keys, elems, assocs
    -- * Lenses
  , work, running, sudoing, repos, tasks
    -- * Initializations
  , initializeSTM
    -- * Auxiliary types
  , Work(..)
  ) where

import Control.Applicative (Applicative(..))
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens
import Data.Functor.Trans.Tagged
import Data.Monoid (mempty)
import Data.Reflection (Reifies)
import Data.List (foldl')
import Data.Set (Set)
import Prelude hiding (lookup)


-- | Convenient type alias for task-local-state-ful IO
-- tagged with crosstask execution environment @s@
type Executor s a = TaggedT s IO a

-- | Get execution environment
env :: (Applicative m, Reifies s a) => TaggedT s m a
env = reflected


-- | 0-to-1 key\/value pairs 'Map'
--
-- @
-- Map k a ~ [(k, a)]@
-- Mip k a ~ Maybe (k, a)
-- @
data Mip k a = Empty | Mip k a
    deriving (Show, Read, Eq, Ord)

instance Functor (Mip k) where
  fmap _ Empty     = Empty
  fmap f (Mip k a) = Mip k (f a)

makePrisms ''Mip

-- | Check if key is here
lookup :: Eq k => k -> Mip k a -> Maybe a
lookup _      Empty = Nothing
lookup k' (Mip k a) = bool Nothing (Just a) (k == k')

-- | Insert value at key if 'Mip' is empty or
-- holds the value of the same key
insert :: Eq k => k -> a -> Mip k a -> Mip k a
insert k a        Empty = Mip k a
insert k a x@(Mip k' _) = bool x (Mip k a) (k == k')

-- | Delete value at key if 'Mip' has it
delete :: Eq k => k -> Mip k a -> Mip k a
delete _        Empty = Empty
delete k' x@(Mip k _) = bool x Empty (k == k')

-- fold for 'Bool', see 'maybe'
bool :: a -> a -> Bool -> a
bool f t p = if p then t else f

type instance Index (Mip k a) = k
type instance IxValue (Mip k a) = a

instance (Applicative f, Eq k) => Ixed f (Mip k a) where
  ix = ixAt

instance Eq k => At (Mip k a) where
  at k f m = indexed f k mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v  -> insert k v m
   where
    mv = lookup k m

-- | Construct 'Mip' from pair
singleton :: k -> a -> Mip k a
singleton = Mip

-- | Construct 'Mip' from '[]'
fromList :: Eq k => [(k, a)] -> Mip k a
fromList = foldl' (\a (k, v) -> insert k v a) Empty

-- | All 0 or 1 'Mip' keys
keys :: Mip k a -> Maybe k
keys Empty     = Nothing
keys (Mip k _) = Just k

-- | All 0 or 1 'Mip' values
elems :: Mip k a -> Maybe a
elems Empty     = Nothing
elems (Mip _ a) = Just a

-- | All 0 or 1 'Mip' key\/value pairs
assocs :: Mip k a -> Maybe (k, a)
assocs Empty     = Nothing
assocs (Mip k v) = Just (k, v)


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
    { _work    = a
    , _running = b
    , _sudoing = c
    , _repos   = d
    , _tasks   = e
    }

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Infinite stream of elements supply monad transformer
module Control.Monad.Stream
  ( -- * Types
    -- ** Monad
    StreamT(..), Stream
    -- ** mtl-style class
  , MonadStream(..)
    -- * Run 'StreamT'
  , runInfiniteStreamT, runStreamT, runStream
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free (Free)
import           Control.Monad.Reader (ReaderT(..), MonadReader(..))
import           Control.Monad.State (StateT(..), MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer (WriterT(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.Stream.Infinite as Infinite
import           Data.Monoid (Monoid)


type Stream e = StreamT e Identity

newtype StreamT e (m :: * -> *) a =
  StreamT { unStreamT :: Infinite.Stream e -> m (Infinite.Stream e, a) }

instance Monad m => Functor (StreamT e m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Monad m => Applicative (StreamT e m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (StreamT e m) where
  return a = StreamT (\xs -> return (xs, a))
  {-# INLINE return #-}
  StreamT x >>= k = StreamT $ \es -> do
    (es', x') <- x es
    unStreamT (k x') es'
  {-# INLINE (>>=) #-}

instance MonadTrans (StreamT e) where
  lift ma = StreamT $ \es -> do
    a <- ma
    return (es, a)
  {-# INLINE lift #-}

-- | Run 'StreamT' with the supplied list. This list is expected to be infinite
--
-- >>> runStreamT [1..] (replicateM 7 next)
-- [1,2,3,4,5,6,7]
runStreamT :: Monad m => [e] -> StreamT e m a -> m a
runStreamT = runInfiniteStreamT . Infinite.fromList
{-# INLINE runStreamT #-}

-- | Run 'StreamT' in the context of 'Identity' monad
runStream :: [e] -> Stream e a -> a
runStream es = runIdentity . runStreamT es
{-# INLINE runStream #-}

-- | Run 'StreamT' with the supplied list. This list is expected to be infinite
runInfiniteStreamT :: Monad m => Infinite.Stream e -> StreamT e m a -> m a
runInfiniteStreamT es sema = liftM snd (unStreamT sema es)
{-# INLINE runInfiniteStreamT #-}

mapStreamT :: (m (Infinite.Stream e, a) -> n (Infinite.Stream e, b)) -> StreamT e m a -> StreamT e n b
mapStreamT f (StreamT g) = StreamT (f . g)


-- | mtl-style class, to avoid manual lifting
class Monad m => MonadStream e m | m -> e where
  -- | Next stream element
  next :: m e
  -- | Peek stream element
  peek :: m e

instance Monad m => MonadStream e (StreamT e m) where
  next = StreamT $ \(e Infinite.:> es) -> return (es, e)
  {-# INLINE next #-}
  peek = StreamT $ \es@(e Infinite.:> _) -> return (es, e)
  {-# INLINE peek #-}

instance (Monad m, MonadStream e m) => MonadStream e (ReaderT r m) where
  next = lift next
  {-# INLINE next #-}
  peek = lift peek
  {-# INLINE peek #-}

instance (Monad m, MonadStream e m) => MonadStream e (StateT s m) where
  next = lift next
  {-# INLINE next #-}
  peek = lift peek
  {-# INLINE peek #-}

instance (Monad m, Monoid w, MonadStream e m) => MonadStream e (WriterT w m) where
  next = lift next
  {-# INLINE next #-}
  peek = lift peek
  {-# INLINE peek #-}

instance (Functor m, MonadStream e m) => MonadStream e (Free m) where
  next = lift next
  {-# INLINE next #-}
  peek = lift peek
  {-# INLINE peek #-}

instance (Monad m, MonadReader r m) => MonadReader r (StreamT e m) where
  ask = lift ask
  {-# INLINE ask #-}
  local = mapStreamT . local
  {-# INLINE local #-}
  reader = lift . reader
  {-# INLINE reader #-}

instance (Monad m, MonadState s m) => MonadState s (StreamT e m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

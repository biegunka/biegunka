{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Infinite stream of unique tokens monad transformer
module Control.Biegunka.Script.Token
  ( -- * Types
    -- ** Monad
    StreamT(..)
    -- ** mtl-style class
  , MonadStream(..)
    -- ** Token stream
  , Infinite(..), Token
    -- * Run 'StreamT'
  , runStreamT
    -- * Tokens
  , tokens, noTokens, fromList
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free (Free)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (StateT(..), MonadState(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (WriterT(..))
import Data.Monoid (Monoid)
import Prelude hiding (head)


newtype StreamT e m a =
  StreamT { unStreamT :: Infinite e -> m (Infinite e, a) }

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

-- | Run 'StreamT' with the supplied stream.
--
-- >>> runStreamT (fromList [1..]) (replicateM 7 next)
-- [1,2,3,4,5,6,7]
runStreamT :: Monad m => Infinite e -> StreamT e m a -> m a
runStreamT es sema = liftM snd (unStreamT sema es)
{-# INLINE runStreamT #-}

mapStreamT :: (m (Infinite e, a) -> n (Infinite e, b)) -> StreamT e m a -> StreamT e n b
mapStreamT f (StreamT g) = StreamT (f . g)


infixr 5 :<
data Infinite a = a :< Infinite a
  deriving (Show, Eq, Ord, Functor)


-- | Get 'Infinite' stream head
--
-- prop> \x -> head (fromList [x..]) == (x :: Integer)
head :: Infinite a -> a
head (a :< _) = a
{-# INLINE head #-}

-- | Get 'Infinite' stream from list
--
-- >>> fromList [1,2,3]
-- 1 :< (2 :< (3 :< *** Exception: Control.Biegunka.Script.Token.fromList: supplied list is not infinite
fromList :: [a] -> Infinite a
fromList = foldr (:<) (error "Control.Biegunka.Script.Token.fromList: supplied list is not infinite")
{-# INLINE fromList #-}


newtype Token = Token Integer
  deriving (Show, Eq, Ord, Enum)

tokens :: Infinite Token
tokens = fromList [Token 0..]

noTokens :: Infinite Token
noTokens = error "sorry, no tokens"


-- | mtl-style class, to avoid manual lifting
class Monad m => MonadStream e m | m -> e where
  -- | Next stream element
  next :: m e
  -- | Peek stream element
  peek :: m e

instance Monad m => MonadStream e (StreamT e m) where
  next = StreamT $ \(e :< es) -> return (es, e)
  {-# INLINE next #-}
  peek = StreamT $ \es -> return (es, head es)
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

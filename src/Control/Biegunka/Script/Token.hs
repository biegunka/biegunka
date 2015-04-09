{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Infinite stream of unique tokens monad transformer
module Control.Biegunka.Script.Token
  ( -- * Types
    -- ** Monad
    StreamT
    -- ** mtl-style class
  , MonadStream(..)
    -- ** Token stream
  , Infinite(..), Token
    -- * Do things with 'StreamT'
  , runStreamT, mapStreamT
    -- * Tokens
  , tokens, noTokens, fromList
  ) where

import Control.Applicative
import Control.Monad
import Data.Void (Void)
import Control.Monad.Free (Free)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (StateT(..), MonadState(..))
import Control.Monad.Trans (MonadTrans(..))
import Prelude hiding (head)


-- | Monad transformer for the never ending stream of stuff
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
-- >>> runStreamT (fromList (map Token [1..])) (replicateM 3 next)
-- [Token 1,Token 2,Token 3]
runStreamT :: Monad m => Infinite e -> StreamT e m a -> m a
runStreamT es sema = liftM snd (unStreamT sema es)
{-# INLINE runStreamT #-}

-- | Map both the value and underlying monad
mapStreamT :: (m (Infinite e, a) -> n (Infinite e, b)) -> StreamT e m a -> StreamT e n b
mapStreamT f (StreamT g) = StreamT (f . g)
{-# INLINE mapStreamT #-}


infixr 5 :<
-- | Never ending stream of stuff
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

-- | Token.
--
-- Supposedly, the only way to get tokens is using 'tokens',
-- so they are somewhat unique
newtype Token = Token Integer
  deriving (Show, Eq, Ord, Enum)

-- | Infinite stream of tokens
tokens :: Infinite Token
tokens = fromList [Token 0 ..]

-- | Infinite stream of /nothing/, very zen
noTokens :: Infinite Void
noTokens = error "Control.Biegunka.Script.Token.noTokens: evaluated"

-- This really should be a closed type family, waiting for GHC 7.8
type family IsToken a :: Bool
type instance IsToken Token = 'True
type instance IsToken Void  = 'False


-- | mtl-style class, to avoid manual lifting
class Monad m => MonadStream e m | m -> e where
  -- | Get next element from the stream; advance that stream by one step
  next :: m e
  -- | Take a look at the next stream element, without modifying the stream
  peek :: m e

instance (IsToken e ~ 'True, Monad m) => MonadStream e (StreamT e m) where
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

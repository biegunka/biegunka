{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | User script type definitions
module Biegunka.Script
  ( Script(..), liftS, annotate, rewind
  , token
  , runScript, evalScript
  ) where

import Control.Applicative (Applicative(..), (<$>))

import Control.Lens
import Control.Monad.Free (Free, iter, liftF)
import Control.Monad.State (MonadState(..), StateT(..), lift, state)
import Data.Default (Default(..))

import Biegunka.Language

type family SA (sc :: Scope) :: *
type instance SA Profiles = Int
type instance SA Sources  = Int
type instance SA Actions  = ()


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script s a = Script { unScript :: StateT SS (Free (EL (SA s) s)) a }

instance Functor (Script s) where
  fmap f (Script m) = Script (fmap f m)
  {-# INLINE fmap #-}

instance Applicative (Script s) where
  pure v = Script (pure v)
  {-# INLINE pure #-}
  Script m <*> Script n = Script (m <*> n)
  {-# INLINE (<*>) #-}

instance Monad (Script s) where
  return v = Script (return v)
  {-# INLINE return #-}
  Script m >>= f = Script (m >>= unScript . f)
  {-# INLINE (>>=) #-}

instance Default a => Default (Script s a) where
  def = return def
  {-# INLINE def #-}

-- | Get DSL and resulting state from 'Script'
runScript :: SS -> Script s a -> Free (EL (SA s) s) (a, SS)
runScript s = (`runStateT` s) . unScript
{-# INLINE runScript #-}

-- | Get DSL from 'Script'
evalScript :: SS -> Script s a -> Free (EL (SA s) s) a
evalScript = (fmap fst .) . runScript
{-# INLINE evalScript #-}

data SS = SS
  { _token :: Int
  } deriving (Show, Read)

instance Default SS where
  def = undefined &
    token .~ 0

-- | Unique token for each 'EP'/'ES'
token :: Lens' SS Int
token f s@(SS { _token = t }) = (\t' -> s { _token = t' }) <$> f t
{-# INLINE token #-}


-- | Lift DSL term to the 'Script'
liftS :: EL (SA s) s a -> Script s a
liftS = Script . lift . liftF
{-# INLINE liftS #-}

-- | Annotate DSL
annotate :: Script s a -> StateT SS (Free (EL (SA t) t)) (Free (EL (SA s) s) a)
annotate i = state $ \s ->
  let r = runScript s i
      ast = fmap fst r
      s' = iter peek $ fmap snd r
  in (ast, s')

-- | Rewind state part pointed by a 'Lens\'' after monadic action execution
rewind :: MonadState s m => Lens' s a -> m b -> m a
rewind l mb = do
  a <- use l
  mb
  a' <- use l
  l .= a
  return a'

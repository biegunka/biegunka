{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | User script type definitions
module Biegunka.Script
  ( Script(..), liftS, annotate
  ) where

import Control.Applicative (Applicative(..))

import Control.Monad.Free (Free, iter, liftF)
import Control.Monad.State (StateT(..), lift)
import Data.Default (Default(..))

import Biegunka.Language

type family SA (sc :: Scope) :: *
type instance SA Profiles = Int
type instance SA Sources  = Int
type instance SA Actions  = ()


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script s a = Script { runScript :: StateT Int (Free (EL (SA s) s)) a }

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
  Script m >>= f = Script (m >>= runScript . f)
  {-# INLINE (>>=) #-}

instance Default a => Default (Script s a) where
  def = return def
  {-# INLINE def #-}


-- | Lift DSL term to the 'Script'
liftS :: EL (SA s) s a -> Script s a
liftS = Script . lift . liftF
{-# INLINE liftS #-}

-- | Annotate DSL
annotate :: Script s a -> Int -> (Free (EL (SA s) s) a, Int)
annotate i s =
  let r = runStateT (runScript i) (s + 1)
      ast = fmap fst r
      s' = iter zoom $ fmap snd r
  in (ast, s')

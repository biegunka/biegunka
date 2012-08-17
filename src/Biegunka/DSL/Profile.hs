-- | Biegunka.Profile module provides profile engine as free monad.
module Biegunka.DSL.Profile
  ( Profile(..)
  , profile
  ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)

import Biegunka.DSL.Repository


-- | Profile engine
data Profile a next =
    Profile String (StateT () (Free (Repository a)) a) next


instance Functor (Profile a) where
  fmap f (Profile name repo next) = Profile name repo (f next)


-- | Sta
profile ∷ String → StateT () (Free (Repository a)) a → Free (Profile a) ()
profile name repo = liftF (Profile name repo ())

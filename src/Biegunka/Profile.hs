-- | Biegunka.Profile module provides profile engine as free monad.
module Biegunka.Profile
  ( Profile(..)
  , profile
  ) where

import Control.Monad.Free (Free(..), liftF)

import Biegunka.Repository


-- | Profile engine
data Profile a next =
    Profile String (Free (Repository a) a) next

instance Functor (Profile a) where
  fmap f (Profile name repo next) = Profile name repo (f next)


-- | Sta
profile ∷ String → Free (Repository a) a → Free (Profile a) ()
profile name repo = liftF (Profile name repo ())

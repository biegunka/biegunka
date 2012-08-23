{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL.Profile
  ( Profile(..)
  , profile
  ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

import Biegunka.State
import Biegunka.DSL.Source


data Profile next =
    Profile String (StateT BiegunkaState (Free Source) ()) next


instance Functor Profile where
  fmap f (Profile name repo next) = Profile name repo (f next)


profile ∷ String → StateT BiegunkaState (Free Source) () → StateT BiegunkaState (Free Profile) ()
profile name repo = lift . liftF $ Profile name repo ()

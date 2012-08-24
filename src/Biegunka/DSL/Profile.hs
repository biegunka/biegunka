{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL.Profile
  ( Profile(..)
  , profile
  ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

import Biegunka.State
import Biegunka.DSL.Source (Source)
import Biegunka.DSL.Files (Files)


data Profile a next =
    Profile String a next


instance Functor (Profile a) where
  fmap f (Profile name repo next) = Profile name repo (f next)


profile ∷ String
        → StateT BiegunkaState (Free (Source (StateT BiegunkaState (Free Files) ()))) ()
        → StateT BiegunkaState (Free (Profile (StateT BiegunkaState (Free (Source (StateT BiegunkaState (Free Files) ()))) ()))) ()
profile name repo = lift . liftF $ Profile name repo ()

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
        → StateT (BiegunkaState s) (Free (Source (StateT (BiegunkaState s) (Free Files) ()))) ()
        → StateT (BiegunkaState s) (Free (Profile (StateT (BiegunkaState s) (Free (Source (StateT (BiegunkaState s) (Free Files) ()))) ()))) ()
profile name repo = lift . liftF $ Profile name repo ()

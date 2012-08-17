-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.DSL.Repository
  ( Repository(..)
  , git
  ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

import Biegunka.DSL.Files


data Repository a next =
    Git String FilePath (StateT () (Free Files) a) next


instance Functor (Repository a) where
  fmap f (Git url path script next) = Git url path script (f next)


git ∷ String → FilePath → StateT () (Free Files) a → StateT () (Free (Repository a)) ()
git url path script = lift . liftF $ Git url path script ()

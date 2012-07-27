-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.Repository
  ( Repository(..)
  , git
  ) where

import Control.Monad.Free (Free(..), liftF)

import Biegunka.Script


-- | Repositories
data Repository a next =
    Git String FilePath (Free Script a) next


instance Functor (Repository a) where
  fmap f (Git url path script next) = Git url path script (f next)


-- | Setup git repository
git ∷ String → FilePath → Free Script a → Free (Repository a) ()
git url path script = liftF (Git url path script ())

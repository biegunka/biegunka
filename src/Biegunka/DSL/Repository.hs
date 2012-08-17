-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.DSL.Repository
  ( Repository(..)
  , git
  ) where

import Control.Lens (use)
import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))

import Biegunka.State
import Biegunka.DSL.Files


data Repository a next =
    Git String FilePath (StateT BiegunkaState (Free Files) a) next


instance Functor (Repository a) where
  fmap f (Git url path script next) = Git url path script (f next)


git ∷ String → FilePath → StateT BiegunkaState (Free Files) a → StateT BiegunkaState (Free (Repository a)) ()
git url path script = do
  sr ← use root
  lift . liftF $ Git url (sr </> path) script ()

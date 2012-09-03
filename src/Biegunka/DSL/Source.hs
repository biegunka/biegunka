{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL.Source
  ( Source(..)
  , git, git_
  ) where

import Control.Lens (use)
import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))

import Biegunka.State
import Biegunka.DSL.Files (Files)


data Source a next =
    Git String FilePath a next


instance Functor (Source a) where
  fmap f (Git url path script next) = Git url path script (f next)


git ∷ String
    → FilePath
    → StateT (BiegunkaState s) (Free Files) ()
    → StateT (BiegunkaState s) (Free (Source (StateT (BiegunkaState s) (Free Files) ()))) ()
git url path script = do
  sr ← use root
  lift . liftF $ Git url (sr </> path) script ()


git_ ∷ String
     → FilePath
     → StateT (BiegunkaState s) (Free (Source (StateT (BiegunkaState s) (Free Files) ()))) ()
git_ url path = git url path (return ())

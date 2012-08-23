{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL.Source
  ( Source(..)
  , git
  ) where

import Control.Lens (use)
import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))

import Biegunka.State
import Biegunka.DSL.Files


data Source next =
    Git String FilePath (StateT BiegunkaState (Free Files) ()) next


instance Functor Source where
  fmap f (Git url path script next) = Git url path script (f next)


git ∷ String → FilePath → StateT BiegunkaState (Free Files) () → StateT BiegunkaState (Free Source) ()
git url path script = do
  sr ← use root
  lift . liftF $ Git url (sr </> path) script ()

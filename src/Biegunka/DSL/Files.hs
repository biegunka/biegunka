{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL.Files
  ( Files(..), Compiler(..)
  , message
  , registerAt
  , copy, link, compile
  ) where

import Control.Lens (use)
import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))

import Biegunka.State


data Compiler = GHC
  deriving Show


data Files next =
    Message String next
  | RegisterAt FilePath FilePath next
  | Link FilePath FilePath next
  | Copy FilePath FilePath next
  | Compile Compiler FilePath FilePath next


instance Functor Files where
  fmap f (Message m next)           = Message m (f next)
  fmap f (RegisterAt src dst next)  = RegisterAt src dst (f next)
  fmap f (Link src dst next)        = Link src dst (f next)
  fmap f (Copy src dst next)        = Copy src dst (f next)
  fmap f (Compile cmp src dst next) = Compile cmp src dst (f next)


message ∷ String → StateT BiegunkaState (Free Files) ()
message m = lift . liftF $ Message m ()


registerAt ∷ FilePath → StateT BiegunkaState (Free Files) ()
registerAt dst = do
  rr ← use repositoryRoot
  sr ← use root
  lift . liftF $ RegisterAt rr (sr </> dst) ()


link ∷ FilePath → FilePath → StateT BiegunkaState (Free Files) ()
link src dst = do
  rr ← use repositoryRoot
  sr ← use root
  lift . liftF $ Link (rr </> src) (sr </> dst) ()


copy ∷ FilePath → FilePath → StateT BiegunkaState (Free Files) ()
copy src dst = do
  rr ← use repositoryRoot
  sr ← use root
  lift . liftF $ Copy (rr </> src) (sr </> dst) ()


compile ∷ Compiler → FilePath → FilePath → StateT BiegunkaState (Free Files) ()
compile cmp src dst = do
  rr ← use repositoryRoot
  sr ← use root
  lift . liftF $ Compile cmp (rr </> src) (sr </> dst) ()

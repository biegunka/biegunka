-- | Biegunka.Files module provides script engine as free monad.
module Biegunka.DSL.Files
  ( Files(..), Compiler(..)
  , message
  , registerAt
  , copy, link, compile
  ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)


data Compiler = GHC
  deriving Show


data Files next =
    Message String next
  | RegisterAt FilePath next
  | Link FilePath FilePath next
  | Copy FilePath FilePath next
  | Compile Compiler FilePath FilePath next


instance Functor Files where
  fmap f (Message m next)           = Message m (f next)
  fmap f (RegisterAt dst next)      = RegisterAt dst (f next)
  fmap f (Link src dst next)        = Link src dst (f next)
  fmap f (Copy src dst next)        = Copy src dst (f next)
  fmap f (Compile cmp src dst next) = Compile cmp src dst (f next)


message ∷ String → StateT () (Free Files) ()
message m = lift $ liftF (Message m ())


registerAt ∷ FilePath → StateT () (Free Files) ()
registerAt dst = lift $ liftF (RegisterAt dst ())


link ∷ FilePath → FilePath → StateT () (Free Files) ()
link src dst = lift $ liftF (Link src dst ())


copy ∷ FilePath → FilePath → StateT () (Free Files) ()
copy src dst = lift $ liftF (Copy src dst ())


compile ∷ Compiler → FilePath → FilePath → StateT () (Free Files) ()
compile cmp src dst = lift $ liftF (Compile cmp src dst ())

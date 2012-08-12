-- | Biegunka.Script module provides script engine as free monad.
module Biegunka.Script
  ( Script(..), Compiler(..)
  , message
  , registerAt
  , copy, link, compile
  ) where

import Control.Monad.Free (Free(..), liftF)


-- | Compilers enumeration
data Compiler = GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


-- | Script engine
data Script next =
    Message String next
  | RegisterAt FilePath next
  | Link FilePath FilePath next
  | Copy FilePath FilePath next
  | Compile Compiler FilePath FilePath next


instance Functor Script where
  fmap f (Message m next)           = Message m (f next)
  fmap f (RegisterAt dst next)      = RegisterAt dst (f next)
  fmap f (Link src dst next)        = Link src dst (f next)
  fmap f (Copy src dst next)        = Copy src dst (f next)
  fmap f (Compile cmp src dst next) = Compile cmp src dst (f next)


-- | Send a message to stdout
message ∷ String → Free Script ()
message m = liftF (Message m ())


-- | Link a repository somewhere
registerAt ∷ FilePath → Free Script ()
registerAt dst = liftF (RegisterAt dst ())


-- | Link a file somewhere
link ∷ FilePath → FilePath → Free Script ()
link src dst = liftF (Link src dst ())


-- | Copy a file somewhere
copy ∷ FilePath → FilePath → Free Script ()
copy src dst = liftF (Copy src dst ())


-- | Compile a file somewhere
compile ∷ Compiler → FilePath → FilePath → Free Script ()
compile cmp src dst = liftF (Compile cmp src dst ())

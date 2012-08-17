-- | Biegunka.Files module provides script engine as free monad.
module Biegunka.DSL.Files
  ( Files(..), Compiler(..)
  , message
  , registerAt
  , copy, link, compile
  ) where

import Control.Monad.Free (Free(..), liftF)


-- | Compilers enumeration
data Compiler = GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


-- | Files engine
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


message ∷ String → Free Files ()
message m = liftF (Message m ())


registerAt ∷ FilePath → Free Files ()
registerAt dst = liftF (RegisterAt dst ())


link ∷ FilePath → FilePath → Free Files ()
link src dst = liftF (Link src dst ())


copy ∷ FilePath → FilePath → Free Files ()
copy src dst = liftF (Copy src dst ())


compile ∷ Compiler → FilePath → FilePath → Free Files ()
compile cmp src dst = liftF (Compile cmp src dst ())

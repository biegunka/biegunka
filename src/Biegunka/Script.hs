-- | Biegunka.Script module provides script engine as free monad.
module Biegunka.Script
  ( Script(..), Compiler(..)
  , message, linkRepo, linkRepoFile
  , copyRepoFile, compile
  ) where

import Control.Monad.Free (Free(..), liftF)


-- | Compilers enumeration
data Compiler = GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


-- | Script engine
data Script next =
    Message String next
  | LinkRepo FilePath next
  | LinkRepoFile FilePath FilePath next
  | CopyRepoFile FilePath FilePath next
  | Compile Compiler FilePath FilePath next


instance Functor Script where
  fmap f (Message m next)            = Message m (f next)
  fmap f (LinkRepo dst next)         = LinkRepo dst (f next)
  fmap f (LinkRepoFile src dst next) = LinkRepoFile src dst (f next)
  fmap f (CopyRepoFile src dst next) = CopyRepoFile src dst (f next)
  fmap f (Compile cmp src dst next)  = Compile cmp src dst (f next)


-- | Send a message to stdout
message ∷ String → Free Script ()
message m = liftF (Message m ())


-- | Link a repository somewhere
linkRepo ∷ FilePath → Free Script ()
linkRepo dst = liftF (LinkRepo dst ())


-- | Link a file somewhere
linkRepoFile ∷ FilePath → FilePath → Free Script ()
linkRepoFile src dst = liftF (LinkRepoFile src dst ())


-- | Copy a file somewhere
copyRepoFile ∷ FilePath → FilePath → Free Script ()
copyRepoFile src dst = liftF (CopyRepoFile src dst ())


-- | Compile a file somewhere
compile ∷ Compiler → FilePath → FilePath → Free Script ()
compile cmp src dst = liftF (Compile cmp src dst ())

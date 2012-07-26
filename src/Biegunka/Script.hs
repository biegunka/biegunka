-- | Biegunka.Script module provides script engine as free monad.
module Biegunka.Script
  ( Free(..), Script(..), Compiler(..)
  , message, linkRepo, linkRepoFile
  , copyRepoFile, compile
  ) where


data Free f r = Free (f (Free f r)) | Pure r


instance (Functor f) ⇒ Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r


-- | Compilers enumeration
data Compiler = GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


-- | Script engine.
data Script next =
    Message String next -- ^ Send a message to stdout
  | LinkRepo FilePath next -- ^ Link a repository somewhere
  | LinkRepoFile FilePath FilePath next -- ^ Link a file somewhere
  | CopyRepoFile FilePath FilePath next -- ^ Copy a file somewhere
  | Compile Compiler FilePath FilePath next -- ^ Compile a file somewhere


instance Functor Script where
  fmap f (Message m next)            = Message m (f next)
  fmap f (LinkRepo dst next)         = LinkRepo dst (f next)
  fmap f (LinkRepoFile src dst next) = LinkRepoFile src dst (f next)
  fmap f (CopyRepoFile src dst next) = CopyRepoFile src dst (f next)
  fmap f (Compile cmp src dst next)  = Compile cmp src dst (f next)


liftF ∷ Functor f ⇒ f r → Free f r
liftF command = Free (fmap Pure command)


message ∷ String → Free Script ()
message m = liftF (Message m ())


linkRepo ∷ FilePath → Free Script ()
linkRepo dst = liftF (LinkRepo dst ())


linkRepoFile ∷ FilePath → FilePath → Free Script ()
linkRepoFile src dst = liftF (LinkRepoFile src dst ())


copyRepoFile ∷ FilePath → FilePath → Free Script ()
copyRepoFile src dst = liftF (CopyRepoFile src dst ())


compile ∷ Compiler → FilePath → FilePath → Free Script ()
compile cmp src dst = liftF (Compile cmp src dst ())

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.DSL
  ( module B
  , FileScript, SourceScript, ProfileScript
  , Profile(..), profile
  , Source(..) , to, from, script, update, step
  , Files(..), Compiler(..), message, registerAt, copy, link, compile
  , Next(..), foldie, mfoldie, transform
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Monoid (Monoid(..))

import Control.Lens (over, makeLenses, use, uses)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import System.FilePath ((</>))

import Biegunka.Settings as B


type Script s α β = StateT (Settings s) (Free α) β


-- Supported compilers
data Compiler =
    GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


data Files next =
    Message String next
  | RegisterAt FilePath FilePath next
  | Link FilePath FilePath next
  | Copy FilePath FilePath next
  | Compile Compiler FilePath FilePath next


-- | Convenient wrapper to hide complexity of types
type FileScript s a = Script s Files a


instance Functor Files where
  fmap f (Message m next)           = Message m (f next)
  fmap f (RegisterAt src dst next)  = RegisterAt src dst (f next)
  fmap f (Link src dst next)        = Link src dst (f next)
  fmap f (Copy src dst next)        = Copy src dst (f next)
  fmap f (Compile cmp src dst next) = Compile cmp src dst (f next)


-- | Prints specified message to stdout
--
-- > message "hello!"
--
-- prints \"hello!\"
message ∷ String → FileScript s ()
message m = lift . liftF $ Message m ()


-- | Source registration
--
-- Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt ∷ FilePath → FileScript s ()
registerAt dst = join $ lifty RegisterAt <$> use sourceRoot <*> uses root (</> dst)


-- | File link
--
-- Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link ∷ FilePath → FilePath → FileScript s ()
link src dst = join $ lifty Link <$> uses sourceRoot (</> src) <*> uses root (</> dst)


-- | File copy
--
-- Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy ∷ FilePath → FilePath → FileScript s ()
copy src dst = join $ lifty Copy <$> uses sourceRoot (</> src) <*> uses root (</> dst)


-- | File compilation
--
-- Compiles given file with given compiler to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   compile GHC "you.hs" "we/need/you/here"
--
-- Compiles ${HOME}\/git\/repo\/you.hs to ${HOME}\/we\/need\/you\/here
compile ∷ Compiler → FilePath → FilePath → FileScript s ()
compile cmp src dst = join $ lifty (Compile cmp) <$> uses sourceRoot (</> src) <*> uses root (</> dst)


lifty ∷ (Functor f, MonadTrans t) ⇒ (c → d → () → f a) → c → d → t (Free f) a
lifty f r sr = lift . liftF $ f r sr ()


data Source a b = Source
  { _from ∷ String
  , _to ∷ FilePath
  , _script ∷ a
  , _update ∷ IO ()
  , _step ∷ b
  }


-- | Convenient wrapper to hide complexity of types
type SourceScript s a = Script s (Source (FileScript s ())) a


makeLenses ''Source


instance Functor (Source a) where
  fmap = over step


data Profile a b = Profile String a b


-- | Convenient wrapper to hide complexity of types
type ProfileScript s a = Script s (Profile (SourceScript s ())) a


instance Functor (Profile a) where
  fmap f (Profile name repo n) = Profile name repo (f n)


-- | Configuration profile
--
-- Provides convenient sources grouping
--
-- > profile "mine" $ do
-- >   git ...
-- >   git ...
-- > profile "friend's" $ do
-- >   svn ...
profile ∷ String → SourceScript s () → ProfileScript s ()
profile name repo = lift . liftF $ Profile name repo ()


class Next f where
  next ∷ f a → a


instance Next Files where
  next (Message _ x) = x
  next (RegisterAt _ _ x) = x
  next (Link _ _ x) = x
  next (Copy _ _ x) = x
  next (Compile _ _ _ x) = x


instance Next (Source a) where
  next (Source _ _ _ _ x) = x


instance Next (Profile a) where
  next (Profile _ _ x) = x


foldie ∷ Next f ⇒ (a → b → b) → b → (f (Free f c) → a) → (Free f c) → b
foldie f a g (Free t) = f (g t) (foldie f a g (next t))
foldie _ a _ (Pure _) = a


mfoldie ∷ (Monoid m, Next f) ⇒ (f (Free f c) → m) → (Free f c) → m
mfoldie = foldie mappend mempty


transform ∷ (f (Free f a) → g (Free g a)) → Free f a → Free g a
transform f (Free t) = Free (f t)
transform _ (Pure t) = Pure t

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.DSL
  ( module B
  , FileScript, SourceScript, ProfileScript
  , Profile, profile
  , Source, Files, from, to, script, update, next
  , Command(..), Compiler(..), message, registerAt, copy, link, compile, substitute
  , foldie, mfoldie, foldieM, foldieM_, transform
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Monoid (Monoid(..))

import Control.Lens (Lens, (^.), over, use, uses)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text.Lazy (Text)
import System.FilePath ((</>))
import Text.StringTemplate (ToSElem, newSTMP, render, setAttribute)
import Text.StringTemplate.GenericStandard ()

import Biegunka.Settings as B


type Script s t α β = StateT (Settings s t) (Free α) β


-- | Convenient wrapper to hide complexity of types
type FileScript s t a = Script s t (Command Files ()) a


-- | Convenient wrapper to hide complexity of types
type SourceScript s t a = Script s t (Command Source (FileScript s t ())) a


-- | Convenient wrapper to hide complexity of types
type ProfileScript s t a = Script s t (Command Profile (SourceScript s t ())) a


data Files
data Source
data Profile

-- Supported compilers
data Compiler =
    GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


data Command l s a where
  Message ∷ String → a → Command Files () a
  RegisterAt ∷ FilePath → FilePath → a → Command Files () a
  Link ∷ FilePath → FilePath → a → Command Files () a
  Copy ∷ FilePath → FilePath → a → Command Files () a
  Compile ∷ Compiler → FilePath → FilePath → a → Command Files () a
  Template ∷ FilePath → FilePath → (String → Text) → a → Command Files () a
  Source ∷ { _from ∷ String, _to ∷ FilePath, _script ∷ s, _update ∷ IO (), _step ∷ a } → Command Source s a
  Profile ∷ String → s → a → Command Profile s a


next ∷ Lens (Command l s a) (Command l s b) a b
next f (Message m x)          = Message m <$> f x
next f (RegisterAt s d x)     = RegisterAt s d <$> f x
next f (Link s d x)           = Link s d <$> f x
next f (Copy s d x)           = Copy s d <$> f x
next f (Compile c s d x)      = Compile c s d <$> f x
next f (Template s d g x)     = Template s d g <$> f x
next f s@(Source {_step = x}) = (\y → s { _step = y }) <$> f x
next f (Profile n s x)        = Profile n s <$> f x
{-# INLINE next #-}


from ∷ Lens (Command Source s a) (Command Source s a) FilePath FilePath
from f s@(Source {_from = x}) = (\y → s { _from = y }) <$> f x
{-# INLINE from #-}


to ∷ Lens (Command Source s a) (Command Source s a) FilePath FilePath
to f s@(Source {_to = x}) = (\y → s { _to = y }) <$> f x
{-# INLINE to #-}


script ∷ Lens (Command Source s a) (Command Source s' a) s s'
script f s@(Source {_script = x}) = (\y → s { _script = y }) <$> f x
{-# INLINE script #-}


update ∷ Lens (Command Source s a) (Command Source s a) (IO ()) (IO ())
update f s@(Source {_update = x}) = (\y → s { _update = y }) <$> f x
{-# INLINE update #-}


instance Functor (Command l s) where
  fmap = over next


-- | Prints specified message to stdout
--
-- > message "hello!"
--
-- prints \"hello!\"
message ∷ String → FileScript s t ()
message m = lift . liftF $ Message m ()


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt ∷ FilePath → FileScript s t ()
registerAt dst = join $ lifty RegisterAt <$> use sourceRoot <*> uses root (</> dst)


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link ∷ FilePath → FilePath → FileScript s t ()
link src dst = join $ lifty Link <$> uses sourceRoot (</> src) <*> uses root (</> dst)


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy ∷ FilePath → FilePath → FileScript s t ()
copy src dst = join $ lifty Copy <$> uses sourceRoot (</> src) <*> uses root (</> dst)


-- | Compiles given file with given compiler to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   compile GHC "you.hs" "we/need/you/here"
--
-- Compiles ${HOME}\/git\/repo\/you.hs to ${HOME}\/we\/need\/you\/here
compile ∷ Compiler → FilePath → FilePath → FileScript s t ()
compile cmp src dst = join $ lifty (Compile cmp) <$> uses sourceRoot (</> src) <*> uses root (</> dst)


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute ∷ ToSElem t ⇒ FilePath → FilePath → FileScript s t ()
substitute src dst = do
  sr ← uses sourceRoot (</> src)
  r ← uses root (</> dst)
  t ← uses template (\b → render . setAttribute "template" b . newSTMP)
  lift . liftF $ Template sr r t ()


lifty ∷ (Functor f, MonadTrans t) ⇒ (c → d → () → f a) → c → d → t (Free f) a
lifty f r sr = lift . liftF $ f r sr ()


-- | Configuration profile
--
-- Provides convenient sources grouping
--
-- > profile "mine" $ do
-- >   git ...
-- >   git ...
-- > profile "friend's" $ do
-- >   svn ...
profile ∷ String → SourceScript s t () → ProfileScript s t ()
profile name repo = lift . liftF $ Profile name repo ()


foldie ∷ (a → b → b) → b → (Command l s (Free (Command l s) c) → a) → (Free (Command l s) c) → b
foldie f a g (Free t) = f (g t) (foldie f a g (t ^. next))
foldie _ a _ (Pure _) = a


mfoldie ∷ Monoid m ⇒ (Command l s (Free (Command l s) c) → m) → (Free (Command l s) c) → m
mfoldie = foldie mappend mempty


foldieM ∷ Monad m ⇒ (Command l s (Free (Command l s) c) → m a) → Free (Command l s) c → m ()
foldieM = foldie (>>) (return ())
{-# SPECIALIZE foldieM ∷ (Command l s (Free (Command l s) c) → IO a) → Free (Command l s) c → IO () #-}


foldieM_ ∷ Monad m ⇒ (Command l s (Free (Command l s) c) → m ()) → Free (Command l s) c → m ()
foldieM_ = foldie (>>) (return ())
{-# SPECIALIZE foldieM_ ∷ (Command l s (Free (Command l s) c) → IO ()) → Free (Command l s) c → IO () #-}


transform ∷ (f (Free f a) → g (Free g a)) → Free f a → Free g a
transform f (Free t) = Free (f t)
transform _ (Pure t) = Pure t

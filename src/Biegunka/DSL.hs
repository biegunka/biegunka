{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.DSL
  ( module B
  , FileScript, SourceScript, ProfileScript
  , Layer(..)
  , next, action, from, to, script, update
  , Command(..), Action(..)
  , Compiler(..), message, registerAt, copy, link, ghc, substitute
  , chmod, chown
  , profile
  , foldie, mfoldie, foldieM, foldieM_, transform
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Monoid (Monoid(..))

import Control.Lens (Lens, (^.), over, query, queries)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text.Lazy (Text)
import System.FilePath ((</>))
import System.Posix.Types (FileMode)
import Text.StringTemplate (ToSElem, newSTMP, render, setAttribute)
import Text.StringTemplate.GenericStandard ()

import Biegunka.Settings as B


type Script s t α β = StateT (Settings s t) (Free α) β


-- | Convenient wrapper to hide complexity of types
type FileScript s t a = ReaderT (Settings s t) (Free (Command Files ())) a


-- | Convenient wrapper to hide complexity of types
type SourceScript s t a = Script s t (Command Source (FileScript s t ())) a


-- | Convenient wrapper to hide complexity of types
type ProfileScript s t a = Script s t (Command Profile (SourceScript s t ())) a


data Layer = Files | Source | Profile

-- Supported compilers
data Compiler =
    GHC -- ^ The Glorious Glasgow Haskell Compilation System
  deriving Show


data Command (l ∷ Layer) s a where
  F ∷ Action → a → Command Files () a
  S ∷ String → FilePath → s → IO () → a → Command Source s a
  P ∷ String → s → a → Command Profile s a
  W ∷ Command l s b → a → Command l s a


instance Functor (Command l s) where
  fmap = over next


next ∷ Lens (Command l s a) (Command l s b) a b
next f (F a x)       = F a <$> f x
next f (S a b c d x) = (\y → S a b c d y) <$> f x
next f (P n s x)     = P n s <$> f x
next f (W s x)       = W s <$> f x
{-# INLINE next #-}


action ∷ Lens (Command Files () a) (Command Files () a) Action Action
action f (F x a) = (\y → F y a) <$> f x
action f (W x a) = (\y → W y a) <$> action f x
{-# INLINE action #-}


from ∷ Lens (Command Source s a) (Command Source s a) FilePath FilePath
from f (S x a b c d) = (\y → S y a b c d) <$> f x
from f (W x a) = (\y → W y a) <$> from f x
{-# INLINE from #-}


to ∷ Lens (Command Source s a) (Command Source s a) FilePath FilePath
to f (S a x b c d) = (\y → S a y b c d) <$> f x
to f (W x a) = (\y → W y a) <$> to f x
{-# INLINE to #-}


script ∷ Lens (Command Source s a) (Command Source s' a) s s'
script f (S a b x c d) = (\y → S a b y c d) <$> f x
script f (W x a) = (\y → W y a) <$> script f x
{-# INLINE script #-}


update ∷ Lens (Command Source s a) (Command Source s a) (IO ()) (IO ())
update f (S a b c x d) = (\y → S a b c y d) <$> f x
update f (W x a) = (\y → W y a) <$> update f x
{-# INLINE update #-}


data Action =
    Message String
  | RegisterAt FilePath FilePath
  | Link FilePath FilePath
  | Copy FilePath FilePath
  | Compile Compiler FilePath FilePath
  | Template FilePath FilePath (String → Text)
  | Mode FilePath FileMode
  | Ownership FilePath String String


-- | Prints specified message to stdout
--
-- > message "hello!"
--
-- prints \"hello!\"
message ∷ String → FileScript s t ()
message m = lift . liftF $ F (Message m) ()


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt ∷ FilePath → FileScript s t ()
registerAt dst = join $ lifty RegisterAt <$> query sourceRoot <*> queries root (</> dst)


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link ∷ FilePath → FilePath → FileScript s t ()
link src dst = join $ lifty Link <$> queries sourceRoot (</> src) <*> queries root (</> dst)


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy ∷ FilePath → FilePath → FileScript s t ()
copy src dst = join $ lifty Copy <$> queries sourceRoot (</> src) <*> queries root (</> dst)


-- | Compiles given file with ghc to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   compile GHC "you.hs" "we/need/you/here"
--
-- Compiles ${HOME}\/git\/repo\/you.hs to ${HOME}\/we\/need\/you\/here
ghc ∷ FilePath → FilePath → FileScript s t ()
ghc src dst = join $ lifty (Compile GHC) <$> queries sourceRoot (</> src) <*> queries root (</> dst)


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute ∷ ToSElem t ⇒ FilePath → FilePath → FileScript s t ()
substitute src dst = do
  sr ← queries sourceRoot (</> src)
  r ← queries root (</> dst)
  t ← queries template (\b → render . setAttribute "template" b . newSTMP)
  lift . liftF $ F (Template sr r t) ()


-- | Changes mode of given file to specified value
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   chmod "bin/script.sh" (ownerModes `unionFileModes` groupReadMode `unionFileModes` otherReadMode)
--
-- Changes file mode of ${HOME}\/bin\/script.sh to 0744
chmod ∷ FilePath → FileMode → FileScript s t ()
chmod fp m = join $ lifty Mode <$> queries root (</> fp) <*> return m


-- | Changes ownership of given file to specified values
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   chown "bin/script.sh" "user" "group"
--
-- Changes ownership of ${HOME}\/bin\/script.sh to user:group
chown ∷ FilePath → String → String → FileScript s t ()
chown fp u g = do
  r ← queries root (</> fp)
  lift . liftF $ F (Ownership r u g) ()


lifty ∷ MonadTrans t ⇒ (a → b → Action) → a → b → t (Free (Command Files ())) ()
lifty f r sr = lift . liftF $ F (f r sr) ()


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
profile name repo = lift . liftF $ P name repo ()


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

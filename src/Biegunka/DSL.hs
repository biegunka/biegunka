{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.DSL
  ( Script, Layer(..)
  , next
  , Command(..), Action(..), Wrapper(..), OnFail(..)
  , Compiler(..), message, registerAt, copy, link, ghc, substitute
  , chmod, chown
  , sudo, ignorant
  , profile
  , foldie, mfoldie, foldieM, foldieM_
  ) where

import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..))

import Control.Lens (Lens, (^.), over)
import Control.Monad.Free (Free(..), liftF)
import Data.Text.Lazy (Text)
import System.Posix.Types (FileMode)
import Text.StringTemplate (ToSElem, newSTMP, render, setAttribute)
import Text.StringTemplate.GenericStandard ()


type family Script (a ∷ Layer)


type instance (Script Files)   = Free (Command Files ()) ()
type instance (Script Source)  = Free (Command Source (Script Files)) ()
type instance (Script Profile) = Free (Command Profile (Script Source)) ()


data Layer = Files | Source | Profile


-- Supported compilers
data Compiler =
    GHC -- ^ The Glorious Glasgow Haskell Compilation System
    deriving Show


data Command (l ∷ Layer) s a where
  F ∷ Action → a → Command l () a
  S ∷ String → FilePath → s → (FilePath → IO ()) → a → Command l s a
  S' ∷ a → Command l s a
  P ∷ String → s → a → Command l s a
  W ∷ Wrapper → a → Command l s a


instance Functor (Command l s) where
  fmap = over next


next ∷ Lens (Command l s a) (Command l s b) a b
next f (F a x)       = F a <$> f x
next f (S a b c d x) = (\y → S a b c d y) <$> f x
next f (S' x)        = (\y → S' y) <$> f x
next f (P n s x)     = P n s <$> f x
next f (W s x)       = W s <$> f x
{-# INLINE next #-}


data Action =
    Message String
  | RegisterAt FilePath FilePath
  | Link FilePath FilePath
  | Copy FilePath FilePath
  | Compile Compiler FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t ⇒ t → String → Text)
  | Mode FilePath FileMode
  | Ownership FilePath String String


data Wrapper =
    User (Maybe String)
  | Ignorance Bool


data OnFail = Ignorant | Ask | Abortive


-- | Prints specified message to stdout
--
-- > message "hello!"
--
-- prints \"hello!\"
message ∷ String → Script Files
message m = liftF $ F (Message m) ()


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt ∷ FilePath → Script Files
registerAt dst = liftF $ F (RegisterAt mempty dst) ()


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link ∷ FilePath → FilePath → Script Files
link src dst = liftF $ F (Link src dst) ()


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy ∷ FilePath → FilePath → Script Files
copy src dst = liftF $ F (Copy src dst) ()


-- | Compiles given file with ghc to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   compile GHC "you.hs" "we/need/you/here"
--
-- Compiles ${HOME}\/git\/repo\/you.hs to ${HOME}\/we\/need\/you\/here
ghc ∷ FilePath → FilePath → Script Files
ghc src dst = liftF $ F (Compile GHC src dst) ()


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute ∷ FilePath → FilePath → Script Files
substitute src dst = liftF $
  F (Template src dst (\b → render . setAttribute "template" b . newSTMP)) ()


-- | Changes mode of given file to specified value
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   chmod "bin/script.sh" (ownerModes `unionFileModes` groupReadMode `unionFileModes` otherReadMode)
--
-- Changes file mode of ${HOME}\/bin\/script.sh to 0744
chmod ∷ FilePath → FileMode → Script Files
chmod fp m = liftF $ F (Mode fp m) ()


-- | Changes ownership of given file to specified values
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   chown "bin/script.sh" "user" "group"
--
-- Changes ownership of ${HOME}\/bin\/script.sh to user:group
chown ∷ FilePath → String → String → Script Files
chown fp u g = liftF $ F (Ownership fp u g) ()


sudo ∷ String → Free (Command l s) () → Free (Command l s) ()
sudo name cs = liftF (W (User (Just name)) ()) >> cs >> liftF (W (User Nothing) ())


ignorant ∷ Free (Command l s) () → Free (Command l s) ()
ignorant cs = liftF (W (Ignorance True) ()) >> cs >> liftF (W (Ignorance False) ())
-- | Configuration profile
--
-- Provides convenient sources grouping
--
-- > profile "mine" $ do
-- >   git ...
-- >   git ...
-- > profile "friend's" $ do
-- >   svn ...
profile ∷ String → Script Source → Script Profile
profile name repo = liftF $ P name repo ()


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

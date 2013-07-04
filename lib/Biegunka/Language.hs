{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
-- | Specifies configuration language
module Biegunka.Language
  ( Scope(..)
  , Term(..), A(..), S(..), P(..), M(..)
  , React(..)
  ) where

import Control.Applicative((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Control.Monad.Free (Free(..))
import Data.Copointed (Copointed(..))
import Data.Text.Lazy (Text)
import System.Process (CmdSpec)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


-- | Language terms scopes [kind]
data Scope = Actions | Sources | Profiles


-- | Language terms datatype.
--
-- "Biegunka.Primitive" contains DSL primitives constructed
-- using these terms.
--
-- User should never construct any DSL term using these.
--
-- Consists of 3 scopes (actions scope, sources scope, and profiles scope)
-- and also scope-agnostic modifiers.
data Term :: (Scope -> *) -> Scope -> * -> * where
  EP :: f Profiles -> P -> Free (Term f Sources) () -> x -> Term f Profiles x
  ES :: f Sources -> S -> Free (Term f Actions) () -> x -> Term f Sources x
  EA :: f Actions -> A -> x -> Term f Actions x
  EM :: M -> x -> Term f s x

instance Functor (Term f s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (Term f s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (Term f s) where
  traverse f (EP a p i x) = EP a p i <$> f x
  traverse f (ES a s i x) = ES a s i <$> f x
  traverse f (EA a z   x) = EA a z   <$> f x
  traverse f (EM   w   x) = EM   w   <$> f x
  {-# INLINE traverse #-}

-- | Peek next 'Term'
instance Copointed (Term f s) where
  copoint (EP _ _ _ x) = x
  copoint (ES _ _ _ x) = x
  copoint (EA _ _   x) = x
  copoint (EM   _   x) = x


-- | 'Profiles' scope datatype
newtype P = P
  { pname :: String -- ^ name
  } deriving (Show, Read, Eq, Ord)

-- | 'Sources' scope data
data S = S {
  -- | Source type
    stype :: String
  -- | URI where source is located
  , suri :: String
  -- | Where to emerge source on FS (relative to Biegunka root setting)
  , spath :: FilePath
  -- | How to update source
  , supdate :: (FilePath -> IO ())
  }

-- | 'Actions' scope datatype
data A =
    -- | Symbolic link
    Link FilePath FilePath
    -- | Verbatim copy
  | Copy FilePath FilePath
    -- | Copy with template substitutions
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
    -- | Shell command
  | Shell FilePath CmdSpec

-- | Modificators for other datatypes
data M =
    User (Maybe String)
  | Reacting (Maybe React)
  | Chain

-- | Failure reaction
data React = Ignorant | Abortive | Retry
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | Specifies configuration language
module Control.Biegunka.Language
  ( Scope(..)
  , Term(..), Action(..), Source(..), Modifier(..)
  , PatchSpec(..), CopySpec(..)
  ) where

import Control.Applicative((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Control.Monad.Free (Free(..))
import Data.Copointed (Copointed(..))
import Data.Default (Default(..))
import Data.Set (Set)
import Data.Text (Text)
import System.Process (CmdSpec)

import Control.Biegunka.Templates


-- | Language terms scopes [kind]
data Scope = Actions | Sources


-- | Language terms datatype.
--
-- "Biegunka.Primitive" contains DSL primitives constructed
-- using these terms (and annotations).
--
-- User should *never* have a need to construct any DSL term using these manually.
--
-- Consists of 2 scopes ('Actions' and 'Sources') and also scope-agnostic modifiers.
data Term :: (Scope -> *) -> Scope -> * -> * where
  TS :: f Sources -> Source -> Free (Term f Actions) () -> x -> Term f Sources x
  TA :: f Actions -> Action -> x -> Term f Actions x
  TM :: Modifier -> x -> Term f s x

instance Functor (Term f s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (Term f s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (Term f s) where
  traverse f (TS a s i x) = TS a s i <$> f x
  traverse f (TA a z   x) = TA a z   <$> f x
  traverse f (TM   w   x) = TM   w   <$> f x
  {-# INLINE traverse #-}

-- | Peek next 'Term'
instance Copointed (Term f s) where
  copoint (TS _ _ _ x) = x
  copoint (TA _ _   x) = x
  copoint (TM   _   x) = x

-- | 'Sources' scope terms data
data Source = Source {
  -- | Source type
    stype :: String
  -- | URI where source is located
  , suri :: String
  -- | Where to emerge source on FS (relative to Biegunka root setting)
  , spath :: FilePath
  -- | How to update source
  , supdate :: FilePath -> IO ()
  }

-- | 'Actions' scope terms data
data Action =
    -- | Symbolic link
    Link FilePath FilePath
    -- | Verbatim copy
  | Copy FilePath FilePath CopySpec
    -- | Copy with template substitutions
  | Template FilePath FilePath (forall t. TemplateSystem t => t -> Text -> Text)
    -- | External command
  | Command FilePath CmdSpec
    -- | Patch
  | Patch FilePath FilePath PatchSpec

-- | Copying settings
data CopySpec =
    OnlyDirectories
  | OnlyFiles
  | BothDirectoriesAndFiles

-- | Patch settings
data PatchSpec = PatchSpec
  { strip     :: Int  -- ^ How many leading slashes to strip
  , reversely :: Bool -- ^ Apply in reverse?
  }

instance Default PatchSpec where
  def = PatchSpec
    { strip     = 1
    , reversely = False
    }

-- | Modificators for other datatypes
data Modifier = Wait (Set Int)

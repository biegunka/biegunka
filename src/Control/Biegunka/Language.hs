{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | Specifies configuration language
module Control.Biegunka.Language
  ( Scope(..)
  , Term(..)
  , Action(..)
  , Source(..)
  , Token(..)
  ) where

#if __GLASGOW_HASKELL__ >= 710
import Data.Traversable (fmapDefault, foldMapDefault)
#else
import Control.Applicative((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)
#endif

import Control.Monad.Free (Free(..))
import Data.Set (Set)
import System.Process (CmdSpec)


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
  TS    :: f 'Sources -> Source -> Free (Term f 'Actions) () -> x -> Term f 'Sources x
  TA    :: f 'Actions -> Action -> x -> Term f 'Actions x
  TWait :: Set Token -> x -> Term f s x

newtype Token = Token Integer
  deriving (Show, Eq, Enum, Ord)

instance Functor (Term f s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (Term f s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (Term f s) where
  traverse f (TS    a s i x) = TS    a s i <$> f x
  traverse f (TA    a z   x) = TA    a z   <$> f x
  traverse f (TWait   w   x) = TWait   w   <$> f x
  {-# INLINE traverse #-}

-- | 'Sources' scope terms data
data Source = Source {
  -- | Source type
    stype :: String
  -- | URI where source is located
  , suri :: String
  -- | Where to emerge source on FS (relative to Biegunka root setting)
  , spath :: FilePath
  -- | How to update source
  , supdate :: FilePath -> IO (Maybe String)
  }

-- | A single action that can be perfomed in the 'Actions' scope.
data Action =
    -- | Symbolically link the file.
    Link FilePath FilePath
    -- | Copy the file verbatim.
  | Copy FilePath FilePath
    -- | Generate the file from the template.
  | Template FilePath FilePath
    -- | Run external command.
  | Command FilePath CmdSpec

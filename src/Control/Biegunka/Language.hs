{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | Specifies configuration language
module Control.Biegunka.Language
  ( Scope(..)
  , Term
  , TermF(..)
  , Action(..)
  , Source(..)
  , Token(..)
  , DiffItem(..)
  , diffItemHeaderOnly
  ) where

import Control.Monad.Free (Free(..))
import Data.Set (Set)
import Data.Traversable (fmapDefault, foldMapDefault)


-- | Language terms scopes [kind]
data Scope = Actions | Sources

type Term f s = Free (TermF f s)

-- | Language terms datatype.
--
-- "Biegunka.Primitive" contains DSL primitives constructed
-- using these terms (and annotations).
--
-- User should *never* have a need to construct any DSL term using these manually.
--
-- Consists of 2 scopes ('Actions' and 'Sources') and also scope-agnostic modifiers.
data TermF :: (Scope -> *) -> Scope -> * -> * where
  TS    :: f 'Sources -> Source -> Free (TermF f 'Actions) () -> x -> TermF f 'Sources x
  TA    :: f 'Actions -> Action -> x -> TermF f 'Actions x
  TWait :: Set Token -> x -> TermF f s x

newtype Token = Token Integer
  deriving (Show, Eq, Enum, Ord)

instance Functor (TermF f s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (TermF f s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (TermF f s) where
  traverse f (TS    a s i x) = TS    a s i <$> f x
  traverse f (TA    a z   x) = TA    a z   <$> f x
  traverse f (TWait   w   x) = TWait   w   <$> f x
  {-# INLINE traverse #-}

-- | 'Sources' scope terms data
data Source = Source
  { sourceType   :: String
  , sourceFrom   :: String
  , sourceTo     :: FilePath
  , sourceUpdate :: FilePath -> IO ([DiffItem], IO [DiffItem])
  }

-- | A single action that can be perfomed in the 'Actions' scope.
data Action =
    -- | Symbolically link the file.
    Link FilePath FilePath
    -- | Copy the file verbatim.
  | Copy FilePath FilePath
    -- | Decrypt the file from the source and metadata.
  | Decrypt FilePath FilePath FilePath
    -- | Generate the file from the template.
  | Template FilePath FilePath
    -- | Run external command.
  | Command FilePath FilePath [String]

data DiffItem = DiffItem
  { diffItemHeader :: String
  , diffItemBody   :: String
  } deriving (Show, Eq)

diffItemHeaderOnly :: String -> DiffItem
diffItemHeaderOnly header =
  DiffItem { diffItemHeader = header, diffItemBody = "" }

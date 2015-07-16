{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Biegunka.Language
  ( Scope(..)
  , Term
  , TermF(..)
  , Source(..)
  , File(..)
  , FileType(..)
  , Command(..)
  , Token(..)
  , DiffItem(..)
  , diffItemHeaderOnly
  , HasOrigin(origin)
  , NoOrigin(..)
  , HasPath(path)
  , NoPath(..)
  , HasMode(mode)
  , (∈)()
  ) where

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Data.Set (Set)
import           Data.Traversable (fmapDefault, foldMapDefault)
import           GHC.Exts (Constraint)
import qualified System.Posix as Posix
import           System.Process (CmdSpec)


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
  TS :: f 'Sources -> Source -> Free (TermF f 'Actions) () -> x -> TermF f 'Sources x
  TF :: f 'Actions -> File type_ FilePath FilePath -> x -> TermF f 'Actions x
  TC :: f 'Actions -> Command -> x -> TermF f 'Actions x
  TW :: Set Token -> x -> TermF f s x

instance Functor (TermF f s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (TermF f s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (TermF f s) where
  traverse f (TS a s i x) = TS a s i <$> f x
  traverse f (TF a z   x) = TF a z   <$> f x
  traverse f (TC a z   x) = TC a z   <$> f x
  traverse f (TW w     x) = TW w     <$> f x
  {-# INLINE traverse #-}

data Source = Source
  { sourceType   :: String
  , sourceFrom   :: String
  , sourceTo     :: FilePath
  , sourceUpdate :: FilePath -> IO ([DiffItem], IO [DiffItem])
  }

data DiffItem = DiffItem
  { diffItemHeader :: String
  , diffItemBody   :: String
  } deriving (Show, Eq)

diffItemHeaderOnly :: String -> DiffItem
diffItemHeaderOnly header =
  DiffItem { diffItemHeader = header, diffItemBody = "" }

data Command = Command
  { commandCwd  :: FilePath
  , commandSpec :: CmdSpec
  }

data File :: FileType -> * -> * -> * where
  FC :: origin -> path -> Posix.FileMode -> File 'Copy origin path
  FT :: origin -> path -> Posix.FileMode -> File 'Template origin path
  FL :: origin -> path -> File 'Link origin path

instance Functor (File t a) where
  fmap f (FC x a y) = FC x (f a) y
  fmap f (FT x a y) = FT x (f a) y
  fmap f (FL x a)   = FL x (f a)

instance Bifunctor (File t) where
  bimap f g (FC a b x) = FC (f a) (g b) x
  bimap f g (FT a b x) = FT (f a) (g b) x
  bimap f g (FL a b)   = FL (f a) (g b)

data FileType = Copy | Template | Link

class HasOrigin s t a b | s -> a, t -> b, a t -> s, b s -> t where
  origin :: Lens s t a b

instance (s ~ t) => HasOrigin (File s a x) (File t b x) a b where
  origin f (FC origin_ path_ mode_) = f origin_ <&> \origin' -> FC origin' path_ mode_
  origin f (FT origin_ path_ mode_) = f origin_ <&> \origin' -> FT origin' path_ mode_
  origin f (FL origin_ path_)       = f origin_ <&> \origin' -> FL origin' path_

data NoOrigin = NoOrigin

class HasPath s t a b | s -> a, t -> b, a t -> s, b s -> t where
  path :: Lens s t a b

instance (s ~ t, x ~ y) => HasPath (File s x a) (File t y b) a b where
  path f (FC origin_ path_ mode_) = f path_ <&> \path' -> FC origin_ path' mode_
  path f (FT origin_ path_ mode_) = f path_ <&> \path' -> FT origin_ path' mode_
  path f (FL origin_ path_)       = f path_ <&> \path' -> FL origin_ path'

data NoPath = NoPath

class HasMode s t a b | s -> a, t -> b, a t -> s, b s -> t where
  mode :: Lens s t a b

instance (s ~ t, t ∈ ['Copy, 'Template]) => HasMode (File s origin path) (File t origin path) Posix.FileMode Posix.FileMode where
  mode f (FC origin_ path_ mode_) = f mode_ <&> \fileMode' -> FC origin_ path_ fileMode'
  mode f (FT origin_ path_ mode_) = f mode_ <&> \fileMode' -> FT origin_ path_ fileMode'
  mode _ _ = error "Should've listened to the exhaustiveness checker."

type family x ∈ xs :: Constraint where
  x ∈ x ': xs = ()
  x ∈ y ': xs = x ∈ xs

newtype Token = Token Integer
  deriving (Show, Eq, Ord)

instance Enum Token where
  toEnum = Token . toEnum
  fromEnum (Token t) = fromEnum t

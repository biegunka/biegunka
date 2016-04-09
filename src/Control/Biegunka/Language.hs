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
  , HasOwner(owner)
  , HasGroup(group)
  , (∈)()
  ) where

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Data.Set (Set)
import           Data.Traversable (fmapDefault, foldMapDefault)
import           GHC.Exts (Constraint)
import qualified System.Directory.Layout as Layout
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
  FC :: origin
     -> path
     -> Maybe Posix.FileMode
     -> Maybe Layout.User
     -> Maybe Layout.Group
     -> File 'Copy origin path
  FT :: origin
     -> path
     -> Maybe Posix.FileMode
     -> Maybe Layout.User
     -> Maybe Layout.Group
     -> File 'Template origin path
  FL :: origin
     -> path
     -> Maybe Layout.User
     -> Maybe Layout.Group
     -> File 'Link origin path

instance Functor (File t a) where
  fmap f (FC a b c d e) = FC a (f b) c d e
  fmap f (FT a b c d e) = FT a (f b) c d e
  fmap f (FL a b d e)   = FL a (f b) d e

instance Bifunctor (File t) where
  bimap f g (FC a b c d e) = FC (f a) (g b) c d e
  bimap f g (FT a b c d e) = FT (f a) (g b) c d e
  bimap f g (FL a b d e)   = FL (f a) (g b) d e

data FileType = Copy | Template | Link

class HasOrigin s t a b | s -> a, t -> b, a t -> s, b s -> t where
  origin :: Lens s t a b

instance (s ~ t, x ~ y) => HasOrigin (File s a x) (File t b x) a b where
  origin f (FC origin_ path_ mode_ owner_ group_) = f origin_ <&> \origin' -> FC origin' path_ mode_ owner_ group_
  origin f (FT origin_ path_ mode_ owner_ group_) = f origin_ <&> \origin' -> FT origin' path_ mode_ owner_ group_
  origin f (FL origin_ path_ owner_ group_)       = f origin_ <&> \origin' -> FL origin' path_ owner_ group_

data NoOrigin = NoOrigin

class HasPath s t a b | s -> a, t -> b, a t -> s, b s -> t where
  path :: Lens s t a b

instance (s ~ t, x ~ y) => HasPath (File s x a) (File t y b) a b where
  path f (FC origin_ path_ mode_ owner_ group_) = f path_ <&> \path' -> FC origin_ path' mode_ owner_ group_
  path f (FT origin_ path_ mode_ owner_ group_) = f path_ <&> \path' -> FT origin_ path' mode_ owner_ group_
  path f (FL origin_ path_ owner_ group_)       = f path_ <&> \path' -> FL origin_ path' owner_ group_

data NoPath = NoPath

class HasMode s t a b | s -> a, t -> b, a t -> s, b s -> t where
  mode :: Lens s t a b

instance (s ~ t, t ∈ ['Copy, 'Template]) => HasMode (File s origin path) (File t origin path) (Maybe Posix.FileMode) (Maybe Posix.FileMode) where
  mode f (FC origin_ path_ mode_ owner_ group_) = f mode_ <&> \mode' -> FC origin_ path_ mode' owner_ group_
  mode f (FT origin_ path_ mode_ owner_ group_) = f mode_ <&> \mode' -> FT origin_ path_ mode' owner_ group_
  mode _ _ = error "Should've listened to the exhaustiveness checker."

class HasOwner s t a b | s -> a, t -> b, a t -> s, b s -> t where
  owner :: Lens s t a b

instance (s ~ t, x ~ y) => HasOwner (File s origin path) (File t origin path) (Maybe Layout.User) (Maybe Layout.User) where
  owner f (FC origin_ path_ mode_ owner_ group_) = f owner_ <&> \owner' -> FC origin_ path_ mode_ owner' group_
  owner f (FT origin_ path_ mode_ owner_ group_) = f owner_ <&> \owner' -> FT origin_ path_ mode_ owner' group_
  owner f (FL origin_ path_ owner_ group_) = f owner_ <&> \owner' -> FL origin_ path_ owner' group_

class HasGroup s t a b | s -> a, t -> b, a t -> s, b s -> t where
  group :: Lens s t a b

instance (s ~ t, x ~ y) => HasGroup (File s origin path) (File t origin path) (Maybe Layout.Group) (Maybe Layout.Group) where
  group f (FC origin_ path_ mode_ owner_ group_) = f group_ <&> \group' -> FC origin_ path_ mode_ owner_ group'
  group f (FT origin_ path_ mode_ owner_ group_) = f group_ <&> \group' -> FT origin_ path_ mode_ owner_ group'
  group f (FL origin_ path_ owner_ group_) = f group_ <&> \group' -> FL origin_ path_ owner_ group'

type family x ∈ xs :: Constraint where
  x ∈ x ': xs = ()
  x ∈ y ': xs = x ∈ xs

newtype Token = Token Integer
  deriving (Show, Eq, Ord)

instance Enum Token where
  toEnum = Token . toEnum
  fromEnum (Token t) = fromEnum t

{-# LANGUAGE FunctionalDependencies #-}
-- | Generally useful 'Sources' related definitions
module Control.Biegunka.Source
  ( Repository
  , HasUrl(..)
  , HasPath(..)
  ) where


type Repository = String

-- | Types containing an URL that can be changed.
class HasUrl s t a | s -> a, t -> a, a s -> t where
  url :: a -> s -> t

-- | Types containing a file path that can be changed.
class HasPath s t a | s -> a, t -> a, a s -> t where
  path :: a -> s -> t

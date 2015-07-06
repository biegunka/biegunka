{-# LANGUAGE FunctionalDependencies #-}
-- | Generally useful 'Sources' related definitions
module Control.Biegunka.Source
  ( Url
  , HasUrl(..)
  , HasPath(..)
  ) where

import Control.Biegunka.Script (Url)


-- | Types containing an URL that can be changed.
class HasUrl s t a | s -> a, t -> a, a s -> t where
  url :: a -> s -> t

-- | Types containing a file path that can be changed.
class HasPath s t a | s -> a, t -> a, a s -> t where
  path :: a -> s -> t

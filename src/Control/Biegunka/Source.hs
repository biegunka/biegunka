{-# LANGUAGE FunctionalDependencies #-}
-- | Generally useful 'Sources' related definitions
module Control.Biegunka.Source
  ( Url
  , HasUrl(..)
  , HasPath(..)
  ) where

import Control.Biegunka.Script (Url)


-- | Types that contain an URL.
--
-- Having only a setter simplifies the interface, so we omit the getter,
-- as the user is not supposed to look into the configuration.
class HasUrl s t a | s -> a, t -> a, a s -> t where
  url :: a -> s -> t

-- | Types that contain a file path.
--
-- Having only a setter simplifies the interface, so we omit the getter,
-- as the user is not supposed to look into the configuration.
class HasPath s t a | s -> a, t -> a, a s -> t where
  path :: a -> s -> t

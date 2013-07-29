{-# LANGUAGE DataKinds #-}
-- | Generally useful 'Sources' related definitions
module Control.Biegunka.Source
  ( Sourceable(..)
  ) where

import Control.Lens

import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Common 'Sources' structure
class Sourceable s where
  -- | Actions to run after source update
  actions :: Lens' s (Script Actions ())

  (==>) :: String -> To -> s -> Script Sources ()

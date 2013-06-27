{-# LANGUAGE DataKinds #-}
-- | Generally useful 'Sources' related definitions
module Biegunka.Source
  ( Source(..)
  ) where

import Control.Lens

import Biegunka.Language
import Biegunka.Script


-- | Common 'Sources' structure
class Source s where
  -- | Actions to run after source update
  actions :: Lens' s (Script Actions ())

  (==>) :: String -> FilePath -> s -> Script Sources ()

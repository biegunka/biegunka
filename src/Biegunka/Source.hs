{-# LANGUAGE DataKinds #-}
module Biegunka.Source
  ( Source(..)
  ) where

import Control.Lens

import Biegunka.Language


-- | Common 'Sources' structure
class Source s where
  -- | Actions to run after source update
  actions :: Lens' s (Script Actions ())

  (==>) :: String -> FilePath -> s -> Script Sources ()

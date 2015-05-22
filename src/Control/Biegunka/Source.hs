{-# LANGUAGE DataKinds #-}
-- | Generally useful 'Sources' related definitions
{-# LANGUAGE TypeFamilies #-}
module Control.Biegunka.Source
  ( Sourceable(..)
  ) where

import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Common 'Sources' structure
class Sourceable s where
  data Mod s

  -- | Actions to run after source update
  actions :: Script 'Actions () -> Mod s

  (==>) :: String -> FilePath -> Mod s -> Script 'Sources ()

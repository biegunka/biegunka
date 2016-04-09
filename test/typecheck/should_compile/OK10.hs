{-# LANGUAGE DataKinds #-}
-- |
-- Clever target path type checking.
--
-- Basic usecases.
module CleverTarget where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.FilePath


aScript :: Script 'Sources ()
aScript = do
  namespace "main" $
    git (origin "git@github.com:ghc/ghc" . path "ghc") pass
  namespace "still-main" $
    git (origin "git@github.com:ghc/ghc" . path ("ghc" </> "chg")) pass
  namespace "not-so-main" $
    git (origin "git@github.com:aghc/aghc" . path (into "ghc")) pass

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
    git (url "git@github.com:ghc/ghc" . path "ghc") (return ())
  namespace "still-main" $
    git (url "git@github.com:ghc/ghc" . path ("ghc" </> "chg")) (return ())
  namespace "not-so-main" $
    git (url "git@github.com:aghc/aghc" . path (into "ghc")) (return ())

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
    git_ (url "git@github.com:ghc/ghc" . path "ghc")
  namespace "still-main" $
    git_ (url "git@github.com:ghc/ghc" . path ("ghc" </> "chg"))
  namespace "not-so-main" $
    git_ (url "git@github.com:aghc/aghc" . path (into "ghc"))

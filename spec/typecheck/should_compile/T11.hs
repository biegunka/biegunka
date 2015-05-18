{-# LANGUAGE DataKinds #-}
-- |
-- Clever target path type checking.
--
-- Basic usecases.
module CleverTarget where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.FilePath


a_script :: Script Sources ()
a_script = do
  namespace "main" $
    git_ "git@github.com:ghc/ghc" "ghc"
  namespace "still-main" $
    git_ "git@github.com:ghc/ghc" ("ghc" </> "chg")
  namespace "not-so-main" $
    git_ "git@github.com:aghc/aghc" (into "ghc")

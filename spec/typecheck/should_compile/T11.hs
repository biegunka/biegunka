{-# LANGUAGE DataKinds #-}
module CleverTarget where
-- |
-- Clever target path type checking.
--
-- Basic usecases.

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.FilePath


some_profile :: Script Sources ()
some_profile = do
  profile "main" $
    git_ "git@github.com:ghc/ghc" "ghc"
  profile "still-main" $
    git_ "git@github.com:ghc/ghc" ("ghc" </> "chg")
  profile "not-so-main" $
    git_ "git@github.com:aghc/aghc" (into "ghc")

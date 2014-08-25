-- |
-- Checks that grouping and sourcing can be on the same 'level' of hierachy
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka id run $ do
  profile "outer" $ do
    group "inner" $
      git_ "git@github.com:ghc/ghc" "ghc"
    git_ "git@github.com:ghc/ghc" "ghc"


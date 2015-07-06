-- |
--
-- Checks that namespaces and sources can be on the "same" level of the hierarchy
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka id run $
  namespace "outer" $ do
    namespace "inner" $
      git_ (url "git@github.com:ghc/ghc" . path "ghc")
    git_ (url "git@github.com:ghc/ghc" . path "ghc")


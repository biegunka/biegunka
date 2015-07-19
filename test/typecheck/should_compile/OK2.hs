-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Sources inside many namespaces
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = do
  biegunka id run $ do
    namespace "main" $
      git (origin "git@github.com:ghc/ghc" . path "ghc") pass
    namespace "other" $
      git (origin "git@github.com:ghc/testsuite" . path "ghc-testsuite") pass
  pass

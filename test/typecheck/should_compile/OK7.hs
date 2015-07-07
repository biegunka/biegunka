-- |
--
-- Checks that namespaces and sources can be on the "same" level of the hierarchy
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = do
  biegunka id run $
    namespace "outer" $ do
      namespace "inner" $
        git (url "git@github.com:ghc/ghc" . path "ghc") pass
      git (url "git@github.com:ghc/ghc" . path "ghc") pass
  pass

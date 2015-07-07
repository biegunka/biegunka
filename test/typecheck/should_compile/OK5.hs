-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source outside any namespaces
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = do
  biegunka id run $
    git (url "git@github.com:ghc/ghc" . path "ghc") pass
  pass

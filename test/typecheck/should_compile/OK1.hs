-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source inside a namespace
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = do
  biegunka id run $
    namespace "main" $
      git (url "git@github.com:ghc/ghc" . path "ghc") pass
  pass

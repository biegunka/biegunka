-- |
-- Checks that sources scope cannot be top-level
module Main where

import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  git_ "git@github.com:ghc/ghc" "ghc"

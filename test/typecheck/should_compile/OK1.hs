-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source inside a namespace
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main = biegunka id run $ do
  namespace "main" $
    git_ "git@github.com:ghc/ghc" "ghc"

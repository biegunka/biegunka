-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source under Profile
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main = biegunka id run $ do
  git_ "git@github.com:ghc/ghc" "ghc"

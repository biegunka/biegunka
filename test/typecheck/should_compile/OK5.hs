-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source outside any namespaces
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka id run $ do
  git_ "git@github.com:ghc/ghc" "ghc"

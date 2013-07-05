-- |
-- Checks that sources scope cannot be top-level
module Main where

import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  git_ "git@github.com:ghc/ghc" "ghc"

-- STDERR
--     Couldn't match type 'Sources with 'Profiles
--     Expected type: Script 'Profiles ()
--       Actual type: Script 'Sources ()

-- |
-- Basic sanity check
--
-- Checks you /can/ have script with Source under Profile
module Main where

import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (run id) $ do
  profile "main" $
    git_ "git@github.com:ghc/ghc" "ghc"

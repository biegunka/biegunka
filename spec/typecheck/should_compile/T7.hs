{-# LANGUAGE OverloadedStrings #-}
-- |
-- Checks that grouping and sourcing can be on the same 'level' of hierachy
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = biegunka id (run id) $ do
  profile "outer" $ do
    group "inner" $
      git_ "git@github.com:ghc/ghc" "ghc"
    git_ "git@github.com:ghc/ghc" "ghc"


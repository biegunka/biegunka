module Main where


import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  profile "main" $
    git_ "git@github.com:ghc/ghc" "ghc"
  profile "other" $
    git_ "git@github.com:ghc/testsuite" "ghc-testsuite"

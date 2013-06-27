module Main where


import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  script = git_ "git@github.com:ghc/ghc" "ghc"

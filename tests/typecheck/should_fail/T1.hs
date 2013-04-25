module Main where


import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id script (execute id)
 where
  script = git_ "git@github.com:ghc/ghc" "ghc"

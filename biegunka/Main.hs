module Main where

import Paths_biegunka_core


main :: IO ()
main = do
  fp <- getDataFileName "data/biegunka-init.template"
  print fp

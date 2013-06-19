module Main where

import System.Directory (copyFile)

import Paths_biegunka_core


main :: IO ()
main = do
  fp <- getDataFileName "data/biegunka-init.template"
  copyFile fp "Main.hs"
  putStrLn "Initialized biegunka script at Biegunka.hs"

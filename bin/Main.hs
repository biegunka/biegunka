module Main (main) where

import System.Environment (getArgs)

import Options (parseArgs)
import App (run)


main :: IO ()
main = run . parseArgs =<< getArgs

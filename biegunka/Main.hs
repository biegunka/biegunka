module Main where

import Data.Char (toLower)
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stdout)

import Paths_biegunka_core


destination :: FilePath
destination = "Main.hs"


main :: IO ()
main = do
  template <- getDataFileName "data/biegunka-init.template"
  destinationExists <- doesFileExist destination
  case destinationExists of
    True -> do
      response <- prompt "Main.hs already exists! Overwrite?"
      case response of
        True  -> initialize template
        False -> do
          putStrLn $ "Failed to initialize biegunka script: Already Exists"
          exitWith (ExitFailure 1)
    False -> initialize template

initialize :: FilePath -> IO ()
initialize source = do
  copyFile source destination
  putStrLn $ "Initialized biegunka script at " ++ destination


prompt :: String -> IO Bool
prompt message = do
  putStr $ message ++ " [y/N] "
  hFlush stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    ""  -> return False
    _   -> prompt message

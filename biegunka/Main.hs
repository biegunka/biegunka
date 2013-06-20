module Main where

import Data.Char (toLower)
import Options.Applicative
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stdout)

import Paths_biegunka_core


data BiegunkaCommand = Init


opts :: ParserInfo BiegunkaCommand
opts = info (helper <*> subcommands) fullDesc
 where
  subcommands = subparser $
    command "init" (info (pure Init) (progDesc "Initialize biegunka script"))


main :: IO ()
main = do
  biegunkaCommand <- customExecParser (prefs showHelpOnError) opts
  case biegunkaCommand of
    Init -> initialize


initialize :: IO ()
initialize = do
  template <- getDataFileName "data/biegunka-init.template"
  destinationExists <- doesFileExist destination
  case destinationExists of
    True -> do
      response <- prompt $ destination ++ " already exists! Overwrite?"
      case response of
        True  -> move template
        False -> do
          putStrLn $ "Failed to initialize biegunka script: Already Exists"
          exitWith (ExitFailure 1)
    False -> move template
 where
  destination :: FilePath
  destination = "Dotfiles.hs"

  move :: FilePath -> IO ()
  move source = do
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

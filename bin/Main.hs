{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Char (toLower)
import           Data.Version (showVersion)
import           Options.Applicative (customExecParser, prefs, showHelpOnError)
import qualified System.Directory as D
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((</>))
import           System.IO (hFlush, hSetBuffering, BufferMode(..), stdout)
import           Text.Printf (printf)

import Paths_biegunka

import Generate (scriptFor)
import List (list)
import Options
import Run (run)

{-# ANN module ("HLint: ignore Use hierarchical imports" :: String) #-}
{-# ANN module ("HLint: ignore Use if" :: String) #-}


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  biegunkaCommand <- customExecParser (prefs showHelpOnError) options
  case biegunkaCommand of
    Init target
      -> defaulted target >>= initialize
    RunScript target args
      -> defaulted target >>= run args
    List datadir format profiles ->
      list datadir profiles format
    GenScript appdir datadir profiles ->
      scriptFor appdir datadir profiles
    Version ->
      printf "biegunka version %s\n" (showVersion version)

-- | Append default biegunka script name if target
-- happens to be a directory
defaulted :: FilePath -> IO FilePath
defaulted target = do
  exists <- D.doesDirectoryExist target
  case exists of
    True  -> return (target </> defaultBiegunkaScriptName)
    False -> return target


initialize :: FilePath -> IO ()
initialize target = do
  template <- getDataFileName "data/biegunka-init.template"
  destinationExists <- D.doesFileExist target
  case destinationExists of
    True -> do
      response <- prompt $ target ++ " already exists! Overwrite?"
      case response of
        True  -> copy template
        False -> do
          putStrLn "Failed to initialize biegunka script: Already Exists"
          exitWith (ExitFailure 1)
    False -> copy template
 where
  copy source = do
    D.copyFile source target
    putStrLn $ "Initialized biegunka script at " ++ target

prompt :: String -> IO Bool
prompt message = do
  putStr $ message ++ " [y/n] "
  hFlush stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    _   -> prompt message

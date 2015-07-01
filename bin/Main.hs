{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Char (toLower)
import           Data.Version (showVersion)
import           Options.Applicative (customExecParser, prefs, showHelpOnError)
import           System.Directory (doesDirectoryExist, doesFileExist, copyFile)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import qualified System.IO as IO
import           Text.Printf (printf)

import qualified Git_biegunka as Git
import qualified Json
import           Options
import           Paths_biegunka (getDataFileName, version)
import           Run (run)


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  biegunkaCommand <- customExecParser (prefs showHelpOnError) options
  case biegunkaCommand of
    Init target
      -> defaulted target >>= initialize
    RunScript target args
      -> defaulted target >>= run args
    Json datadir ->
      Json.out datadir
    Version ->
      printf "biegunka %s-%s\n" (showVersion version) Git.hash

-- | Append default biegunka script name if target
-- happens to be a directory
defaulted :: FilePath -> IO FilePath
defaulted target =
  doesDirectoryExist target >>= \case
    True  -> return (target </> defaultBiegunkaScriptName)
    False -> return target


initialize :: FilePath -> IO ()
initialize target = do
  template <- getDataFileName "data/Biegunka.hs"
  doesFileExist target >>= \case
    True -> prompt (target ++ " already exists! Overwrite?") >>= \case
      True  -> copy template
      False -> do
        IO.hPutStrLn IO.stderr "Failed to initialize biegunka script: Already Exists"
        exitFailure
    False -> copy template
 where
  copy source = do
    copyFile source target
    putStrLn ("Initialized biegunka script at " ++ target)

prompt :: String -> IO Bool
prompt message = do
  putStr (message ++ " [y/n] ")
  IO.hFlush IO.stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    _   -> prompt message

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Char (toLower)
import qualified Data.List as List
import           System.Directory (doesFileExist, copyFile)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.FilePath (combine)
import qualified System.IO as IO
import qualified System.Posix as Posix
import           Text.Printf (printf)

import qualified Json
import           Options
import qualified Paths_biegunka as Paths
import           Run (runScript, findScript)


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  command <- fmap Options.parse getArgs
  case command of
    Right (Init target) -> initialize target
    Right (Run (Just script) args) -> runScript script args
    Right (Run Nothing args) ->
      findScript >>= \case
        [script] -> runScript script args
        [] -> die "No scripts were found in the tree."
        scripts -> die . List.intercalate "\n" $
          ["Found several scripts:"] ++
          map ("  " ++) scripts ++
          ["Please, pass the one to run as an argument."]
    Right (Json datadir) -> Json.out datadir
    Right (Version version) -> putStrLn version
    Right (Help help) -> putStrLn help
    Left help -> die help


initialize :: FilePath -> IO ()
initialize dir = do
  template <- Paths.getDataFileName "data/Biegunka.hs"
  doesFileExist script >>= \case
    False ->
      do copyFile template script
         Posix.setFileMode script mode
         putStrLn (printf "Initialized biegunka script at ‘%s’" script)
    True -> prompt (printf "‘%s’ already exists! Overwrite?" script) >>= \case
      False -> die "Failed to initialize biegunka script: Already Exists"
      True ->
        do copyFile template script
           Posix.setFileMode script mode
           putStrLn (printf "Re-initialized biegunka script at ‘%s’" script)
 where
  script = combine dir scriptName
  mode =
    List.foldl' Posix.unionFileModes
                Posix.nullFileMode
                [Posix.ownerReadMode, Posix.ownerWriteMode, Posix.groupReadMode, Posix.otherReadMode]

prompt :: String -> IO Bool
prompt message = do
  putStr (message ++ " [y/n] ")
  IO.hFlush IO.stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    _   -> prompt message

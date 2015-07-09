{-# LANGUAGE LambdaCase #-}
module Init
  ( init
  ) where

import           Data.Char (toLower)
import qualified Data.List as List
import           Prelude hiding (init)
import           System.Directory (doesFileExist, copyFile)
import           System.FilePath (combine)
import           System.Exit (die)
import qualified System.IO as IO
import qualified System.Posix as Posix
import           Text.Printf (printf)

import           Options (scriptName)
import qualified Paths_biegunka as Paths



init :: FilePath -> IO ()
init dir = do
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

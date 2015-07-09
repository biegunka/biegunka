{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Init
  ( init
  , askUser
  ) where

import           Data.Function (fix)
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Prelude hiding (init)
import           System.Directory (doesFileExist, copyFile)
import           System.FilePath (combine)
import           System.Exit (die)
import qualified System.IO as IO
import qualified System.Posix as Posix
import           Text.Printf (printf)

import           Options (scriptName)



init :: FilePath -> (Text -> IO Bool) -> IO FilePath -> IO ()
init dir prompt getTemplate =
  doesFileExist script >>= \case
    False ->
      do replace script =<< getTemplate
         putStrLn (printf "Initialized biegunka script at ‘%s’" script)
    True -> prompt (Text.pack (printf "‘%s’ already exists! Overwrite?" script)) >>= \case
      False -> die "Failed to initialize biegunka script: Already Exists"
      True ->
        do replace script =<< getTemplate
           putStrLn (printf "Re-initialized biegunka script at ‘%s’" script)
 where
  script = combine dir scriptName

replace :: FilePath -> FilePath -> IO ()
replace dst src =
  do copyFile src dst; Posix.setFileMode dst mode
 where
  mode =
    List.foldl' Posix.unionFileModes
                Posix.nullFileMode
                [Posix.ownerReadMode, Posix.ownerWriteMode, Posix.groupReadMode, Posix.otherReadMode]

askUser :: Text -> IO Bool
askUser question = fix $ \loop ->
  do Text.putStr (question <> " [y/n] ")
     IO.hFlush IO.stdout
     reply <- fmap Text.toLower Text.getLine
     case reply of
       "y"   -> return True
       "yes" -> return True
       "n"   -> return False
       "no"  -> return False
       _     -> loop

{-# LANGUAGE DataKinds #-}
-- | Very simple 'Source' using some existing directory
module Control.Biegunka.Source.Directory (directory) where

import Control.Monad (unless)

import qualified Data.Text as T
import           System.Directory (doesDirectoryExist)

import Control.Biegunka.Execute.Exception (sourceFailure)
import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Use the directory located as specified by first argument as 'Source'
directory
  :: FilePath
  -> Script Actions ()
  -> Script Sources ()
directory relpath inner =
  sourced "directory" url relpath inner update
 where
  update abspath = do
    exists <- doesDirectoryExist abspath
    unless exists $
      sourceFailure url abspath (T.pack "No directory found here!")

  url = "localhost"

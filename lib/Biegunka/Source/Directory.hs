{-# LANGUAGE DataKinds #-}
-- | Very simple 'Source' using some existing directory
module Biegunka.Source.Directory (directory) where

import Control.Monad (unless)

import qualified Data.Text as T
import           System.Directory (doesDirectoryExist)

import Biegunka.Execute.Exception (sourceFailure)
import Biegunka.Language
import Biegunka.Script


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

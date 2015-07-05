{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Very simple 'Source' using some existing directory
module Control.Biegunka.Source.Directory (directory) where

import Control.Monad (unless)
import System.Directory (doesDirectoryExist)

import Control.Biegunka.Execute.Exception (sourceFailure)
import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Use the directory located as specified by first argument as 'Source'
directory
  :: FilePath
  -> Script 'Actions ()
  -> Script 'Sources ()
directory relpath = sourced Source
  { sourceType   = "directory"
  , sourceFrom   = relpath
  , sourceTo     = relpath
  , sourceUpdate = update
  }
 where
  update abspath = do
    exists <- doesDirectoryExist abspath
    unless exists
           (sourceFailure "No directory found!")
    return (Nothing, return Nothing)

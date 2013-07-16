{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Biegunka custom exceptions
module Biegunka.Execute.Exception
  ( -- * Exceptions
    CopyingException(..)
  , PatchingException(..)
  , ShellException(..)
  , SourceException(..)
    -- * Utility functions
  , sourceFailure
  ) where

import Control.Exception (Exception, throwIO)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import System.Process (CmdSpec(..))

import           Data.Text (Text)
import qualified Data.Text as T


-- | Copying files/directories failure (with catched IO exception)
data CopyingException = CopyingException FilePath FilePath String
    deriving (Typeable)

instance Show CopyingException where
  show (CopyingException source destination ioerror) = nicely $
       "Copying "
    <> T.pack source
    <> " to "
    <> T.pack destination
    <> " has failed.\nExceptions log:\n"
    <> T.pack ioerror

instance Exception CopyingException


-- | Patching directory failure
data PatchingException = PatchingException FilePath FilePath
    deriving (Typeable)

instance Show PatchingException where
  show (PatchingException patch root) = nicely $
       "Patch "
    <> T.pack patch
    <> " has failed to apply at "
    <> T.pack root

instance Exception PatchingException


-- | Various shell failures with output
data ShellException = ShellException CmdSpec Text
    deriving (Typeable)

instance Show ShellException where
  show (ShellException (ShellCommand c) o) = nicely $
    "Shell command `" <> T.pack c <> "' has failed\nExceptions log:\n" <> o
  show (ShellException (RawCommand c as) o) = nicely $
    "Command `" <> T.pack c <> " " <> T.pack (intercalate " " as) <> "` has failed\nExceptions log:\n" <> o

instance Exception ShellException


-- | Source emerging failure with paths and output
data SourceException = SourceException String FilePath Text
    deriving (Typeable)

instance Show SourceException where
  show (SourceException up fp fs) = nicely $
    "Biegunka has failed to update source " <> T.pack up <> " at " <> T.pack fp <> "\nExceptions log:\n" <> fs

instance Exception SourceException


-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: String -> FilePath -> Text -> IO a
sourceFailure up fp fs = throwIO $ SourceException up fp fs

nicely :: Text -> String
nicely f = T.unpack . T.unlines . filter (not . T.null) $ T.lines f

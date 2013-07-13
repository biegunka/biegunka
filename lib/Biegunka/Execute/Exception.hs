{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Define Biegunka exceptions
module Biegunka.Execute.Exception
  ( -- * Exceptions
    BiegunkaException(..)
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


-- | Custom exceptions
data BiegunkaException =
    PatchFailure FilePath FilePath     -- ^ Various shell failures with output
  | ShellCommandFailure CmdSpec Text   -- ^ Various shell failures with output
  | SourceFailure String FilePath Text -- ^ Source emerging failure with paths and output
    deriving (Typeable)

instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
   where
    pretty (PatchFailure patch root) =
         "Patch "
      <> T.pack patch
      <> " has failed to apply at "
      <> T.pack root
    pretty (ShellCommandFailure (ShellCommand c) o) =
      "Shell command `" <> T.pack c <> "' has failed\nFailures log:\n" <> o
    pretty (ShellCommandFailure (RawCommand c as) o) =
      "Command `" <> T.pack c <> " " <> T.pack (intercalate " " as) <> "` has failed\nFailures log:\n" <> o
    pretty (SourceFailure up fp fs) =
      "Biegunka has failed to update source " <> T.pack up <> " at " <> T.pack fp <> "\nFailures log:\n" <> fs

instance Exception BiegunkaException


-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: String -> FilePath -> Text -> IO a
sourceFailure up fp fs = throwIO $ SourceFailure up fp fs

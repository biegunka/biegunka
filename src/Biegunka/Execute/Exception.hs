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
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)

import           Data.Text (Text)
import qualified Data.Text as T


-- | Custom exceptions
data BiegunkaException =
    ShellCommandFailure String Text    -- ^ Various shell failures with output
  | SourceFailure String FilePath Text -- ^ Source emerging failure with paths and output
    deriving (Data, Typeable)

instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
   where
    pretty (ShellCommandFailure t o) =
      "Shell command `" <> T.pack t <> "` has failed\nFailures log:\n" <> o
    pretty (SourceFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs

instance Exception BiegunkaException


-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: String -> FilePath -> Text -> IO a
sourceFailure up fp fs = throwIO $ SourceFailure up fp fs

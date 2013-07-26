{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Biegunka custom exceptions
module Control.Biegunka.Execute.Exception
  ( -- * Exceptions
    CopyingException(..)
  , PatchingException(..)
  , ShellException(..)
  , SourceException(..)
    -- * Utility functions
  , sourceFailure
  , onFailure
  ) where

import Control.Applicative ((<$), pure)
import Control.Exception (Exception, throwIO)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
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
data ShellException = ShellException CmdSpec Int Text
    deriving (Typeable)

instance Show ShellException where
  show (ShellException spec status errors) = nicely $
    let commandLine = case spec of
          ShellCommand c  -> ["Shell command >>>", T.pack c, "<<<"]
          RawCommand c as -> ["Command >>>", T.pack c] ++ map T.pack as ++ ["<<<"]
        statusLine  = ["exited with status", T.pack (show status)]
    in T.unlines [T.unwords (commandLine <> statusLine), errors]

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

-- | Check process exit code and perform 'IO' action on failure
onFailure :: ExitCode -> (Int -> IO a) -> IO ()
onFailure (ExitFailure s) f = () <$ f s
onFailure ExitSuccess _     = pure ()

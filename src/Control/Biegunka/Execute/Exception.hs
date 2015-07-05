{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Biegunka custom exceptions
module Control.Biegunka.Execute.Exception
  ( -- * Exceptions
    CopyingException(..)
  , ShellException(..)
  , SourceException(..)
  , _SourceException
    -- * Utility functions
  , sourceFailure
  , onFailure
  ) where

import           Control.Exception (SomeException, Exception, IOException, throwIO)
import           Control.Exception.Lens (exception)
import           Control.Lens
import           Control.Monad (void)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           System.Exit (ExitCode(..))


-- | Copying files/directories failure (with catched IO exception)
newtype CopyingException = CopyingException IOException
    deriving (Typeable)

instance Show CopyingException where
  show (CopyingException exn) = nicely ("Copying failed:\n" <> T.pack (show exn))

instance Exception CopyingException


-- | Various shell failures with output
data ShellException = ShellException Int Text
    deriving (Typeable)

instance Show ShellException where
  show (ShellException status errors) = nicely $
    T.unlines [T.unwords ["Exit status:", T.pack (show status)], "Standard error:", errors]

instance Exception ShellException


-- | Source emerging failure with paths and output
data SourceException = SourceException
  { unSourceException :: Text
  } deriving (Typeable)

instance Show SourceException where
  show (SourceException fs) = nicely ("Update failed:\n" <> fs)

instance Exception SourceException

_SourceException :: Prism' SomeException Text
_SourceException = exception.iso unSourceException SourceException

-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: Text -> IO a
sourceFailure = throwIO . SourceException

nicely :: Text -> String
nicely = T.unpack . T.unlines . filter (not . T.null) . T.lines

-- | Check process exit code and perform 'IO' action on failure
onFailure :: ExitCode -> (Int -> IO a) -> IO ()
onFailure (ExitFailure s) f = void (f s)
onFailure ExitSuccess _     = return ()

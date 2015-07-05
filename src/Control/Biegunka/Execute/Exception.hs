{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Biegunka custom exceptions
module Control.Biegunka.Execute.Exception
  ( -- * Exceptions
    ShellException(..)
  , SourceException(..)
  , _SourceException
    -- * Utility functions
  , sourceFailure
  , onFailure
  ) where

import           Control.Exception (SomeException, Exception, throwIO)
import           Control.Exception.Lens (exception)
import           Control.Lens
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           System.Exit (ExitCode(..))


-- | Various shell failures with output
data ShellException = ShellException Int Text
    deriving (Typeable)

instance Show ShellException where
  show (ShellException status errors) = T.unpack $
    T.unlines [T.unwords ["Exit status:", T.pack (show status)], "Standard error:", errors]

instance Exception ShellException


-- | Source emerging failure with paths and output
newtype SourceException = SourceException
  { unSourceException :: String
  } deriving (Typeable)

instance Show SourceException where
  show (SourceException fs) = "Update failed:\n" ++ fs

instance Exception SourceException

_SourceException :: Prism' SomeException String
_SourceException = exception.iso unSourceException SourceException

-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: String -> IO a
sourceFailure = throwIO . SourceException

-- | Check process exit code and perform 'IO' action on failure
onFailure :: ExitCode -> (Int -> IO a) -> IO ()
onFailure (ExitFailure s) f = void (f s)
onFailure ExitSuccess _     = return ()

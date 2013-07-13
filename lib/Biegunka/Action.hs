{-# LANGUAGE NamedFieldPuns #-}
-- | Abstracted 'Actions' layer routines.
module Biegunka.Action
  ( applyPatch, verifyAppliedPatch
  ) where

import Control.Exception (throwIO)
import System.Exit (ExitCode(..))
import System.IO (IOMode(..), openFile)
import System.Process (runProcess, waitForProcess)

import Biegunka.Execute.Exception
import Biegunka.Language


patching :: FilePath -> FilePath -> [String] -> (ExitCode -> IO a) -> IO a
patching patch root arguments post = do
  stdin   <- openFile patch ReadMode
  stdout  <- openFile "/dev/null" WriteMode
  process <- runProcess "git"
    ("apply" : arguments)
    (Just root)
    Nothing
    (Just stdin) (Just stdout) (Just stdout)
  status  <- waitForProcess process
  post status

applyPatch :: FilePath -> FilePath -> PatchSpec -> IO ()
applyPatch patch root PatchSpec { strip, reversely } =
  patching patch root arguments post
 where
  arguments   = ["-p", show strip] ++ if reversely then ["--reverse"] else []

  post ExitSuccess     = return ()
  post (ExitFailure _) = throwIO $ PatchFailure patch root

verifyAppliedPatch :: FilePath -> FilePath -> PatchSpec -> IO Bool
verifyAppliedPatch patch root PatchSpec { strip, reversely } =
  patching patch root arguments post
 where
  arguments   = ["--check", "-p", show strip] ++ if reversely then [] else ["--reverse"]

  post status = return (status == ExitSuccess)

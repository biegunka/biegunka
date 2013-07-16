{-# LANGUAGE NamedFieldPuns #-}
-- | Abstracted 'Actions' layer routines.
module Biegunka.Action
  ( applyPatch, verifyAppliedPatch
  , copy, verifyCopy
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad (mplus)
import           Control.Exception (throwIO)
import           Data.Traversable (for)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.IO (IOMode(..), openFile)
import           System.Process (runProcess, waitForProcess)

import Biegunka.Execute.Exception
import Biegunka.Language


-- | Generic patching function
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

-- | Apply patch given the patch spec
applyPatch
  :: FilePath  -- ^ Patch location
  -> FilePath  -- ^ Patching root
  -> PatchSpec
  -> IO ()
applyPatch patch root PatchSpec { strip, reversely } =
  patching patch root arguments post
 where
  arguments   = ["-p", show strip] ++ if reversely then ["--reverse"] else []

  post ExitSuccess     = return ()
  post (ExitFailure _) = throwIO $ PatchFailure patch root

-- | Verify applied patch given the patch spec
verifyAppliedPatch
  :: FilePath  -- ^ Patch location
  -> FilePath  -- ^ Patching root
  -> PatchSpec
  -> IO Bool
verifyAppliedPatch patch root PatchSpec { strip, reversely } =
  patching patch root arguments post
 where
  arguments   = ["--check", "-p", show strip] ++ if reversely then [] else ["--reverse"]

  post status = return (status == ExitSuccess)


copy :: FilePath -> FilePath -> CopySpec -> IO ()
copy = undefined

verifyCopy :: FilePath -> FilePath -> CopySpec -> IO Bool
verifyCopy x y spec = case spec of
  OnlyDirectories -> do
    True <- doesDirectoryExist x
    True <- doesDirectoryExist y
    verifyCopyDirectory x y
   `mplus`
    return False
  OnlyFiles -> do
    True <- doesFileExist x
    True <- doesFileExist y
    verifyCopyFile x y
   `mplus`
    return False
  BothDirectoriesAndFiles -> do
    verifyCopyDirectory x y
  `mplus`
    verifyCopyFile x y
  `mplus`
    return False
 where
  verifyCopyFile :: FilePath -> FilePath -> IO Bool
  verifyCopyFile file file' = do
    True <- doesFileExist file
    True <- doesFileExist file'
    True <- liftA2 (==) (B.readFile file) (B.readFile file')
    return True

  verifyCopyDirectory :: FilePath -> FilePath -> IO Bool
  verifyCopyDirectory directory directory' = do
    True      <- doesDirectoryExist directory
    True      <- doesDirectoryExist directory'
    contents  <- getDirectoryContents directory
    contents' <- getDirectoryContents directory'
    case contents == contents' of
      False -> return False
      True  -> do
        for (filter (`notElem` [".", ".."]) contents) $ \subfile ->
          let path  = directory  </> subfile
              path' = directory' </> subfile
          in do
          True <- verifyCopyDirectory path path'
          return True
         `mplus` do
          True <- verifyCopyFile path path'
          return True
        return True

{-# LANGUAGE NamedFieldPuns #-}
-- | Abstracted 'Actions' layer routines.
module Control.Biegunka.Action
  ( applyPatch, verifyAppliedPatch
  , copy, verifyCopy
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad (mplus)
import           Control.Exception (throwIO)
import           Data.Foldable (for_)
import           Data.Traversable (for)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import qualified System.Directory as D
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.IO (IOMode(..), openFile)
import           System.IO.Error (catchIOError)
import           System.Process (runProcess, waitForProcess)

import Control.Biegunka.Execute.Exception
import Control.Biegunka.Language


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
  patching patch root arguments $ \e ->
    e `onFailure` \_ -> throwIO $ PatchingException patch root
 where
  arguments   = ["-p", show strip] ++ if reversely then ["--reverse"] else []

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
copy source destination spec = case spec of
  OnlyFiles ->
    copyFile source destination
  OnlyDirectories ->
    copyDirectory source destination
  BothDirectoriesAndFiles -> do
    file <- doesFileExist source
    case file of
      False -> copyDirectory source destination
      True  -> copyFile source destination
 where
  copyFile :: FilePath -> FilePath -> IO ()
  copyFile file file' = do
    exists <- doesFileExist file
    case exists of
      False -> throwIO (CopyingException file file' ("File " ++ file ++ "does not exist"))
      True  -> D.copyFile file file'
   `catchIOError`
    \exn -> throwIO (CopyingException file file' (show exn))

  copyDirectory :: FilePath -> FilePath -> IO ()
  copyDirectory directory directory' = do
    exists <- doesDirectoryExist directory
    case exists of
      False ->
        throwIO (CopyingException directory directory' ("Directory " ++ directory ++ "does not exist"))
      True -> do
        D.createDirectory directory'
        contents <- getDirectoryContents directory
        for_ (filter (`notElem` [".", ".."]) contents) $ \subpath -> do
          let path  = directory  </> subpath
              path' = directory' </> subpath
          file <- doesFileExist path
          case file of
            False -> copyDirectory path path'
            True  -> copyFile path path'
   `catchIOError`
    \exn -> throwIO (CopyingException directory directory' (show exn))


verifyCopy :: FilePath -> FilePath -> CopySpec -> IO Bool
verifyCopy source destination spec = case spec of
  OnlyDirectories -> do
    True <- doesDirectoryExist source
    True <- doesDirectoryExist destination
    verifyCopyDirectory source destination
   `mplus`
    return False
  OnlyFiles -> do
    True <- doesFileExist source
    True <- doesFileExist destination
    verifyCopyFile source destination
   `mplus`
    return False
  BothDirectoriesAndFiles -> do
    verifyCopyDirectory source destination
  `mplus`
    verifyCopyFile source destination
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

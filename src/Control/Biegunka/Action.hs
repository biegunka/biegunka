{-# LANGUAGE NamedFieldPuns #-}
-- | Abstracted 'Actions' layer routines.
module Control.Biegunka.Action
  ( copy, verifyCopy
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad (mplus)
import           Control.Exception (throwIO)
import           Data.Foldable (for_)
import           Data.Traversable (for)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO.Error (catchIOError)

import           Control.Biegunka.Execute.Exception
import           Control.Biegunka.Language

{-# ANN module "HLint: ignore Use if" #-}


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
      False -> throwIO (CopyingException file file' ("File " ++ file ++ " does not exist"))
      True  -> D.copyFile file file'
   `catchIOError`
    \exn -> throwIO (CopyingException file file' (show exn))

  copyDirectory :: FilePath -> FilePath -> IO ()
  copyDirectory directory directory' = do
    exists <- doesDirectoryExist directory
    case exists of
      False ->
        throwIO (CopyingException directory directory' ("Directory " ++ directory ++ " does not exist"))
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
  BothDirectoriesAndFiles ->
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

module Control.Biegunka.Execute.IO
  ( compareContents
  , compareFileMode
  , hash
  , prepareDestination
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard, void)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Crypto.Hash as Hash
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           System.FilePath (dropFileName)
import qualified System.Directory as D
import qualified System.IO.Error as IO
import qualified System.Posix as Posix

import qualified Control.Biegunka.Patience as Patience

{-# ANN module "HLint: ignore Use const" #-}


-- | Compare the contents of two files. Returns @Just (Left digest)@ if
-- the destination does not exist, @Just (Right (digestSource, digestDestination))@
-- if both files exist and are readable but differ, and @Nothing@ if both files
-- exist, are readable and their contents are the same.
compareContents
  :: Hash.HashAlgorithm a
  => proxy a
  -> FilePath             -- ^ source
  -> FilePath             -- ^ destination, may not exist
  -> IO (Maybe (Either (Hash.Digest a)
                       (Hash.Digest a, Hash.Digest a, Patience.FileDiff)))
compareContents _ src dst = do
  x  <- hash src
  my <- handleDoesNotExist (return Nothing) (fmap Just (hash dst))
  case my of
    Nothing -> return (Just (Left x))
    Just y
      | x /= y -> do
        d <- Patience.fileDiff dst src -- `src` is a new `dst`
        return (Just (Right (x, y, d)))
      | otherwise -> return Nothing

compareFileMode :: FilePath -> Posix.FileMode -> IO (Maybe (Either Posix.FileMode (Posix.FileMode, Posix.FileMode)))
compareFileMode src dstMode = do
  srcMode_ <- handleDoesNotExist (return Nothing)
                                 (fmap (Just . Posix.fileMode) (Posix.getFileStatus src))
  return $ case srcMode_ of
    Nothing -> Just (Left dstMode)
    Just srcMode
      | srcMode /= dstMode -> Just (Right (srcMode, dstMode))
      | otherwise -> Nothing

-- | Create a directory for a file with a given filepath to reside in and
-- unlink the filepath if there's a resident already.
prepareDestination :: FilePath -> IO ()
prepareDestination fp = void $ do
  D.createDirectoryIfMissing True (dropFileName fp)
  IO.tryIOError (Posix.removeLink fp)

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist h =
  handleJust (guard . IO.isDoesNotExistError) (\_ -> h)

-- | Take a hashing algorithm and a filepath and produce a digest.
hash :: Hash.HashAlgorithm a => FilePath -> IO (Hash.Digest a)
hash fp =
  runResourceT (sourceFile fp $$ go Hash.hashInit)
 where
  go x =
    maybe (return $! Hash.hashFinalize x)
          (\bs -> go $! Hash.hashUpdate x bs)
      =<< await

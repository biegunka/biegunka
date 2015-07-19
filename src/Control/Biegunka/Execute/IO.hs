{-# LANGUAGE TypeFamilies #-}
module Control.Biegunka.Execute.IO
  ( ContentsDiff
  , diffContents
  , showContentsDiff
  , FileModeDiff
  , diffFileMode
  , showFileModeDiff
  , OwnerDiff
  , diffOwner
  , showOwnerDiff
  , GroupDiff
  , diffGroup
  , showGroupDiff
  , hash
  , prepareDestination
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard, void)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as ByteString
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Numeric (showOct)
import           System.FilePath (dropFileName)
import qualified System.Directory as D
import qualified System.Directory.Layout as Layout
import qualified System.IO.Error as IO
import qualified System.Posix as Posix
import           Text.Printf (printf)

import           Control.Biegunka.Execute.Describe (prettyDiff)
import           Control.Biegunka.Language (DiffItem(..), diffItemHeaderOnly)
import qualified Control.Biegunka.Patience as Patience

{-# ANN module "HLint: ignore Use const" #-}


newtype ContentsDiff = ContentsDiff (Maybe (Either (Hash.Digest Hash.SHA1) (Hash.Digest Hash.SHA1, Hash.Digest Hash.SHA1, Patience.FileDiff)))
    deriving (Show, Eq)

-- | Compare the contents of two files. Returns @Just (Left digest)@ if
-- the destination does not exist, @Just (Right (digestSource, digestDestination))@
-- if both files exist and are readable but differ, and @Nothing@ if both files
-- exist, are readable and their contents are the same.
diffContents
  :: FilePath -- ^ source
  -> FilePath -- ^ destination, may not exist
  -> IO ContentsDiff
diffContents src dst = do
  x  <- hash src
  my <- handleDoesNotExist (return Nothing) (fmap Just (hash dst))
  case my of
    Nothing -> return (ContentsDiff (Just (Left x)))
    Just y
      | x /= y -> do
        d <- Patience.fileDiff dst src -- `src` is a new `dst`
        return (ContentsDiff (Just (Right (x, y, d))))
      | otherwise -> return (ContentsDiff Nothing)

showContentsDiff :: ContentsDiff -> Maybe DiffItem
showContentsDiff (ContentsDiff diff) = fmap go diff
 where
  go =
    either (diffItemHeaderOnly . printf "contents changed from ‘none’ to ‘%s’" . showHash)
           (\(x, y, d) -> DiffItem
             { diffItemHeader = printf "contents changed from ‘%s' to ‘%s’" (showHash x) (showHash y)
             , diffItemBody   = prettyDiff d
             })
  showHash = take 8 . ByteString.unpack . Hash.digestToHexByteString

newtype FileModeDiff = FileModeDiff (Maybe (Either Posix.FileMode (Posix.FileMode, Posix.FileMode)))
    deriving (Show, Eq)

diffFileMode :: FilePath -> Posix.FileMode -> IO FileModeDiff
diffFileMode src dstMode = do
  srcMode_ <- handleDoesNotExist (return Nothing)
                                 (fmap (Just . Posix.fileMode) (Posix.getFileStatus src))
  return . FileModeDiff $ case srcMode_ of
    Nothing -> Just (Left dstMode)
    Just srcMode
      | srcMode /= dstMode -> Just (Right (srcMode, dstMode))
      | otherwise -> Nothing

showFileModeDiff :: FileModeDiff -> Maybe DiffItem
showFileModeDiff (FileModeDiff diff) = fmap go diff
 where
  go =
    either (diffItemHeaderOnly . printf "file mode changed from ‘none’ to ‘%s’" . showMode)
           (\(x, y) -> diffItemHeaderOnly (printf "file mode changed from ‘%s' to ‘%s’" (showMode x) (showMode y)))
  showMode oct = showOct (oct `mod` 0o1000) ""

newtype OwnerDiff = OwnerDiff ()
    deriving (Show, Eq)

diffOwner :: FilePath -> Layout.User -> IO OwnerDiff
diffOwner = undefined

showOwnerDiff :: OwnerDiff -> Maybe DiffItem
showOwnerDiff = undefined

newtype GroupDiff = GroupDiff ()
    deriving (Show, Eq)

diffGroup :: FilePath -> Layout.Group -> IO GroupDiff
diffGroup = undefined

showGroupDiff :: GroupDiff -> Maybe DiffItem
showGroupDiff = undefined

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

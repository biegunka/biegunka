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
  , prepareDestination
  , hash
  , setOwner
  , setGroup
  ) where

import           Control.Exception (handleJust)
import           Control.Monad (guard, void)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as ByteString
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Data.Bifunctor (bimap)
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
    either (diffItemHeaderOnly . showHeader "none" . showHash)
           (\(x, y, d) -> DiffItem
             { diffItemHeader = showHeader (showHash x) (showHash y)
             , diffItemBody   = prettyDiff d
             })
  showHeader = printf "contents changed from ‘%s’ to ‘%s’"
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
  go = diffItemHeaderOnly
    . uncurry (printf "file mode changed from ‘%s’ to ‘%s’")
    . either (\x -> ("none", showMode x)) (bimap showMode showMode)
  showMode oct = showOct (oct `mod` 0o1000) ""

newtype OwnerDiff = OwnerDiff (Maybe (Either Layout.User (Layout.User, Layout.User)))
    deriving (Show, Eq)

diffOwner :: FilePath -> Layout.User -> IO OwnerDiff
diffOwner src owner = do
  ownerId_ <- handleDoesNotExist (return Nothing)
                                 (fmap (Just . Posix.fileOwner) (Posix.getFileStatus src))
  case ownerId_ of
    Nothing -> return (OwnerDiff (Just (Left owner)))
    Just ownerId -> case owner of
      Layout.UserID uId
        | ownerId /= uId -> return (OwnerDiff (Just (Right (Layout.UserID ownerId, owner))))
        | otherwise -> return (OwnerDiff Nothing)
      Layout.Username uName -> do
        ownerName <- getUserNameForId ownerId
        return . OwnerDiff $ if ownerName /= uName then
          Just (Right (Layout.Username ownerName, owner))
        else
          Nothing

showOwnerDiff :: OwnerDiff -> Maybe DiffItem
showOwnerDiff (OwnerDiff diff) = fmap go diff
 where
  go = diffItemHeaderOnly
    . uncurry (printf "owner changed from ‘%s’ to ‘%s’")
    . either (\x -> ("none", showUser x)) (bimap showUser showUser)
  showUser (Layout.UserID u) = show u
  showUser (Layout.Username u) = u

newtype GroupDiff = GroupDiff (Maybe (Either Layout.Group (Layout.Group, Layout.Group)))
    deriving (Show, Eq)

diffGroup :: FilePath -> Layout.Group -> IO GroupDiff
diffGroup src group = do
  groupId_ <- handleDoesNotExist (return Nothing)
                                 (fmap (Just . Posix.fileGroup) (Posix.getFileStatus src))
  case groupId_ of
    Nothing -> return (GroupDiff (Just (Left group)))
    Just groupId -> case group of
      Layout.GroupID gId
        | groupId /= gId -> return (GroupDiff (Just (Right (Layout.GroupID groupId, group))))
        | otherwise -> return (GroupDiff Nothing)
      Layout.Groupname gName -> do
        groupName <- getGroupNameForId groupId
        return . GroupDiff $ if groupName /= gName then
          Just (Right (Layout.Groupname groupName, group))
        else
          Nothing

showGroupDiff :: GroupDiff -> Maybe DiffItem
showGroupDiff (GroupDiff diff) = fmap go diff
 where
  go = diffItemHeaderOnly
    . uncurry (printf "group changed from ‘%s’ to ‘%s’")
    . either (\x -> ("none", showGroup x)) (bimap showGroup showGroup)
  showGroup (Layout.GroupID g) = show g
  showGroup (Layout.Groupname g) = g

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

setOwner :: FilePath -> Layout.User -> IO ()
setOwner fp (Layout.UserID uid) = Posix.setOwnerAndGroup fp uid (-1)
setOwner fp (Layout.Username name) = getUserIdForName name >>= \uid -> Posix.setOwnerAndGroup fp uid (-1)

setGroup :: FilePath -> Layout.Group -> IO ()
setGroup fp (Layout.GroupID gid) = Posix.setOwnerAndGroup fp (-1) gid
setGroup fp (Layout.Groupname name) = getGroupIdForName name >>= \gid -> Posix.setOwnerAndGroup fp (-1) gid

getUserNameForId :: Posix.UserID -> IO String
getUserNameForId =
  fmap Posix.userName . Posix.getUserEntryForID

getGroupNameForId :: Posix.GroupID -> IO String
getGroupNameForId =
  fmap Posix.groupName . Posix.getGroupEntryForID

getUserIdForName :: String -> IO Posix.UserID
getUserIdForName =
  fmap Posix.userID . Posix.getUserEntryForName

getGroupIdForName :: String -> IO Posix.GroupID
getGroupIdForName =
  fmap Posix.groupID . Posix.getGroupEntryForName

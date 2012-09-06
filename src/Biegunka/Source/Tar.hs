{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Tar - function to work with [.tar, .tar.gz, .tar.bz2] files as sources
module Biegunka.Source.Tar
  ( -- * Source layer
    tar, tar_
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Codec.Compression.BZip as BZip (decompress)
import           Control.Lens ((^.), uses)
import           Control.Monad (when)
import           Control.Monad.Free (liftF)
import           Control.Monad.Trans (lift)
import           Network.Curl.Download.Lazy (openLazyURI)
import           System.FilePath ((</>))
import           System.FilePath.Lens (extension)
import           System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)


-- | Download and extract tar archive from the given url to specified path.
-- Also executes attached script
--
-- > tar "https://example.com/archive.tar.gz" "git/archive" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/git\/archive
--
--  * link ${HOME}\/git\/archive to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/git\/archive\/important.file to ${HOME}\/.config
tar ∷ String → FilePath → FileScript s () → SourceScript s ()
tar u p script = do
  sr ← uses root (</> p)
  lift . liftF $ Source u sr script (update u sr) ()


-- | Download and extract tar archive from the given url to specified path.
--
-- > tar_ "https://example.com/archive.tar.gz" "git/archive" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/git\/archive
tar_ ∷ String → FilePath → SourceScript s ()
tar_ u p = tar u p $ return ()


update ∷ String → FilePath → IO ()
update u p = do
  whenM doesDirectoryExist removeDirectoryRecursive p
  whenM doesFileExist removeFile p
  Tar.unpack p . Tar.read . decompress . either (error . show) id =<< openLazyURI u
 where
  decompress = case u ^. extension of
    ".gz" → GZip.decompress
    ".bz2" → BZip.decompress
    _ → id

  whenM cM f t = cM t >>= \c → when c (f t)

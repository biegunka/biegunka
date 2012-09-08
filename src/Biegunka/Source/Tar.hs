{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Tar - functions to work with [.tar, .tar.gz, .tar.bz2] archives as sources
module Biegunka.Source.Tar
  ( -- * Source layer
    tar, tar_
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Codec.Compression.BZip as BZip (decompress)
import           Control.Lens ((^.), uses)
import           Control.Monad.Free (liftF)
import           Control.Monad.Trans (lift)
import           System.FilePath ((</>))
import           System.FilePath.Lens (extension)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)
import Biegunka.Source.Common (download, remove)


-- | Download and extract tar archive (possibly with compression)
-- from the given url to specified path. Also executes attached script
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
tar url path script = uses root (</> path) >>= \sr → lift . liftF $ Source url sr script (update url sr) ()


-- | Download and extract tar archive (possibly with compression)
-- from the given url to specified path.
--
-- > tar_ "https://example.com/archive.tar.gz" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/git\/archive
tar_ ∷ String → FilePath → SourceScript s ()
tar_ url path = tar url path $ return ()


update ∷ String → FilePath → IO ()
update url path = do
  remove path
  Tar.unpack path . Tar.read . decompress =<< download url
 where
  decompress = case url ^. extension of
    ".gz" → GZip.decompress
    ".bz2" → BZip.decompress
    _ → id

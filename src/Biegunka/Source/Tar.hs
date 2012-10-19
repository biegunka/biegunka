{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Tar - functions to work with [.tar, .tar.gz, .tar.bz2] archives as sources
module Biegunka.Source.Tar
  ( -- * Source layer
    tar, tar_
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Codec.Compression.BZip as BZip (decompress)
import           Control.Lens ((^.))
import           Control.Monad.Free (liftF)
import           Data.ByteString.Lazy (ByteString)
import           System.FilePath.Lens (extension)

import Biegunka.DSL (Script, Layer(Files, Source), Command(S))
import Biegunka.Source.Common (update)


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
tar ∷ String → FilePath → Script Files → Script Source
tar url path script = liftF $ S url path script (updateTar url) ()


-- | Download and extract tar archive (possibly with compression)
-- from the given url to specified path.
--
-- > tar_ "https://example.com/archive.tar.gz" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/git\/archive
tar_ ∷ String → FilePath → Script Source
tar_ url path = tar url path $ return ()


updateTar ∷ String → FilePath → IO ()
updateTar url path = update url path (Tar.unpack path . Tar.read . decompress url)


decompress ∷ String → ByteString → ByteString
decompress url = case url ^. extension of
  ".gz" → GZip.decompress
  ".bz2" → BZip.decompress
  _ → id

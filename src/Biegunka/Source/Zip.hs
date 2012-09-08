{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Zip - functions to work with .zip archives as sources
module Biegunka.Source.Zip
  ( -- * Source layer
    zip, zip_
  ) where

import Control.Applicative ((<$>))
import Prelude hiding (zip)

import Codec.Archive.Zip (toArchive, extractFilesFromArchive)
import Control.Lens (uses)
import Control.Monad.Free (liftF)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)
import Biegunka.Source.Common (download, remove)


-- | Download and extract zip archive from the given url to specified path.
-- Also executes attached script
--
-- > zip "https://example.com/archive.zip" "git/archive" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
--
--  * link ${HOME}\/git\/archive to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/git\/archive\/important.file to ${HOME}\/.config
zip ∷ String → FilePath → FileScript s () → SourceScript s ()
zip url path script = uses root (</> path) >>= \sr → lift . liftF $ Source url sr script (update url sr) ()


-- | Download and extract zip archive from the given url to specified path.
--
-- > zip_ "https://example.com/archive.zip" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
zip_ ∷ String → FilePath → SourceScript s ()
zip_ url path = zip url path $ return ()


update ∷ String → FilePath → IO ()
update url path = do
  remove path
  archive ← toArchive <$> download url
  withWorkingDirectory path $
    extractFilesFromArchive [] archive


withWorkingDirectory ∷ FilePath → IO a → IO ()
withWorkingDirectory path action = do
  saved ← getCurrentDirectory
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
  setCurrentDirectory saved

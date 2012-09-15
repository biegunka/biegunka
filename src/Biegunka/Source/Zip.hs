{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Zip - functions to work with .zip archives as sources
module Biegunka.Source.Zip
  ( -- * Source layer
    zip, zip_
  ) where

import Prelude hiding (zip)

import Codec.Archive.Zip (toArchive, extractFilesFromArchive)
import Control.Lens (uses)
import Control.Monad.Free (liftF)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Command(S), SourceScript)
import Biegunka.Source.Common (update)


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
zip ∷ String → FilePath → FileScript s t () → SourceScript s t ()
zip url path script = uses root (</> path) >>= \sr → lift . liftF $ S url sr script (updateZip url sr) ()


-- | Download and extract zip archive from the given url to specified path.
--
-- > zip_ "https://example.com/archive.zip" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
zip_ ∷ String → FilePath → SourceScript s t ()
zip_ url path = zip url path $ return ()


updateZip ∷ String → FilePath → IO ()
updateZip url path = update url path (with path . extractFilesFromArchive [] . toArchive)


with ∷ FilePath → IO a → IO ()
with path action = do
  saved ← getCurrentDirectory
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
  setCurrentDirectory saved

{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Zip - functions to work with .zip archives as sources
module Biegunka.Source.Zip
  ( -- * Source layer
    zip, zip_
  ) where

import Control.Applicative ((<$>))
import Prelude hiding (zip)

import Codec.Archive.Zip (toArchive, extractFilesFromArchive)
import Control.Lens ((^.), uses)
import Control.Monad (when)
import Control.Monad.Free (liftF)
import Control.Monad.Trans (lift)
import Network.Curl.Download.Lazy (openLazyURI)
import System.FilePath ((</>))
import System.FilePath.Lens (directory)
import System.Directory
  ( doesDirectoryExist, doesFileExist
  , createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory
  , removeDirectoryRecursive, removeFile
  )

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)


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
zip u p script = do
  sr ← uses root (</> p)
  lift . liftF $ Source u sr script (update u sr) ()


-- | Download and extract zip archive from the given url to specified path.
--
-- > zip_ "https://example.com/archive.zip" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
zip_ ∷ String → FilePath → SourceScript s ()
zip_ u p = zip u p $ return ()


update ∷ String → FilePath → IO ()
update u p = do
  whenM doesDirectoryExist removeDirectoryRecursive p
  whenM doesFileExist removeFile p
  archive ← toArchive . either (error . show) id <$> openLazyURI u
  withWorkingDirectory p $
    extractFilesFromArchive [] archive
 where
  whenM cM f t = cM t >>= \c → when c (f t)


withWorkingDirectory ∷ FilePath → IO a → IO ()
withWorkingDirectory path action = do
  saved ← getCurrentDirectory
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
  setCurrentDirectory saved

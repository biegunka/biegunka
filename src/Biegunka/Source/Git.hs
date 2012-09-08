{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Git - functions to work with git repositories as sources
module Biegunka.Source.Git
  ( -- * Source layer
    git, git_
  ) where

import Control.Applicative ((<$>), (<*>))

import Control.Lens (uses)
import Control.Monad (unless)
import Control.Monad.Free (liftF)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Process (runProcess, waitForProcess)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)


-- | Clone repository from the given url to specified path
-- and/or pull from master. Also executes attached script
--
-- > git "https://example.com/repository.git" "git/repository" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * clone repository from https:\/\/example.com\/repository.git to ${HOME}\/git\/repository
--
--  * pull from master
--
--  * link ${HOME}\/git\/repository to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/git\/repository\/important.file to ${HOME}\/.config
git ∷ String → FilePath → FileScript s () → SourceScript s ()
git url path script = uses root (</> path) >>= \sr -> lift . liftF $ Source url sr script (update url sr) ()


-- | Clone repository from the given url to specified path
-- and/or pull from master
--
-- > git_ "https://example.com/repository.git" "git/repository"
--
--  * clone repository from https:\/\/example.com\/repository.git to ${HOME}\/git\/repository
--
--  * pull from master
git_ ∷ String → FilePath → SourceScript s ()
git_ url path = git url path (return ())


update ∷ String → FilePath → IO ()
update url path = do
  exists ← (||) <$> doesDirectoryExist path <*> doesFileExist path
  unless exists $ do
    waitForProcess =<< runProcess "git" ["clone", url, path] Nothing Nothing Nothing Nothing Nothing
    return ()
  waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just path) Nothing Nothing Nothing Nothing
  return ()

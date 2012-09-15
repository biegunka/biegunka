{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Git - functions to work with git repositories as sources
module Biegunka.Source.Git
  ( -- * Source layer
    git, git_
  ) where

import           Control.Lens (uses)
import           Control.Monad (unless)
import           Control.Monad.Free (liftF)
import           Control.Monad.Trans (lift)
import qualified Data.Text.Lazy.IO as T
import           System.FilePath ((</>))
import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode(..))
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process (runProcess, waitForProcess)

import Biegunka.DSL (FileScript, Command(S), SourceScript)
import Biegunka.Settings
import Biegunka.Interpreter.IO (sourceFailure)


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
git ∷ String → FilePath → FileScript s t () → SourceScript s t ()
git url path script = uses root (</> path) >>= \sr → lift . liftF $ S url sr script (update url sr) ()


-- | Clone repository from the given url to specified path
-- and/or pull from master
--
-- > git_ "https://example.com/repository.git" "git/repository"
--
--  * clone repository from https:\/\/example.com\/repository.git to ${HOME}\/git\/repository
--
--  * pull from master
git_ ∷ String → FilePath → SourceScript s t ()
git_ url path = git url path (return ())


update ∷ String → FilePath → IO ()
update url path = do
  (ifd,ofd) ← createPipe
  ih ← fdToHandle ifd
  oh ← fdToHandle ofd
  exists ← doesDirectoryExist path
  unless exists $ do
    check ih =<< waitForProcess =<< runProcess "git" ["clone", url, path] Nothing Nothing Nothing (Just oh) (Just oh)
  check ih =<< waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just path) Nothing Nothing (Just oh) (Just oh)
 where
  check ih (ExitFailure _) = do
    l ← T.hGetContents ih
    sourceFailure url path l
  check _ _ = return ()

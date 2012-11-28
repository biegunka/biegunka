{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Git - functions to work with git repositories as sources
module Biegunka.Source.Git
  ( -- * Source layer
    git, git_
  ) where

import           Control.Monad (unless)
import           Control.Monad.Free (liftF)
import qualified Data.Text.IO as T
import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode(..))
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process (runProcess, waitForProcess)

import Biegunka.DSL (Script, Layer(Files, Source), Command(S))
import Biegunka.Execute (sourceFailure)


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
git ∷ String → FilePath → Script Files () → Script Source ()
git url path script = liftF $ S url path script (updateGit url) ()


-- | Clone repository from the given url to specified path
-- and/or pull from master
--
-- > git_ "https://example.com/repository.git" "git/repository"
--
--  * clone repository from https:\/\/example.com\/repository.git to ${HOME}\/git\/repository
--
--  * pull from master
git_ ∷ String → FilePath → Script Source ()
git_ url path = git url path (return ())


updateGit ∷ String → FilePath → IO ()
updateGit url path = do
  exists ← doesDirectoryExist path
  unless exists $ do
    gitie ["clone", url, path] Nothing
  gitie ["pull", "origin", "master"] (Just path)
 where
  check ih (ExitFailure _) = do
    l ← T.hGetContents ih
    sourceFailure url path l
  check _ _ = return ()

  gitie xs p = do
    (ifd,ofd) ← createPipe
    ih ← fdToHandle ifd
    oh ← fdToHandle ofd
    check ih =<< waitForProcess =<< runProcess "git" xs p Nothing Nothing (Just oh) (Just oh)

module Biegunka.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, doesFileExist, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))

import Biegunka.Core

data Git = Git { url ∷ FilePath, repo ∷ FilePath }

git ∷ FilePath → FilePath → Git
git = Git

instance Repository Git where
  clone (Git u r) = do
    exists ← (||) <$> (doesDirectoryExist r) <*> (doesFileExist r)
    if exists
      then return False
      else (== ExitSuccess) <$> rawSystem "git" ["clone", u, r]
  update (Git _ r) = do
    setCurrentDirectory r
    (== ExitSuccess) <$> rawSystem "git" ["pull", "origin", "master"]
  path = repo

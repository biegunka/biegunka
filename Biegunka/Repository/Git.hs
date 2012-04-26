{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.Repository.Git
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(ExitSuccess))

import Biegunka.Repository (Repository(..))

type UrlPath = String
data Git = Git { url ∷ String, repo ∷ FilePath }

git ∷ UrlPath → FilePath → Git
git = Git

instance Repository Git where
  clone = gitClone
  update = gitPull
  path = gitPath

gitClone ∷ Git → IO Bool
gitClone (Git u r) = do
  exists ← (||) <$> (doesDirectoryExist r) <*> (doesFileExist r)
  if exists
    then return False
    else (== ExitSuccess) <$> rawSystem "git" ["clone", u, r]

gitPull ∷ Git → IO Bool
gitPull = undefined
--gitPull _ = (== ExitSuccess) <$> rawSystem "git" ["pull", "origin", "master"]

gitPath ∷ Git → String
gitPath = repo

-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, doesFileExist, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))

import Biegunka.Core

data Git = Git FilePath FilePath

-- | Setup git instance as follows:
--
-- 1. If specified directory doesn't exist then clone repository from specified URL to it.
--
-- 2. If specified directory does exist then pull from origin master.
--
-- 3. Return ADT with specified repository root path.
git ∷ FilePath → FilePath → IO Git
git u p = clone r >>= flip unless (update r >>= flip unless (error "Biegunka: Something went wrong! Use Biegunka.DryRun to get more info.")) >> return r
  where r = Git u p

instance Repository Git where
  clone (Git u r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    if exists
      then return False
      else (== ExitSuccess) <$> rawSystem "git" ["clone", u, r]
  update (Git _ r) = do
    setCurrentDirectory r
    (== ExitSuccess) <$> rawSystem "git" ["pull", "origin", "master"]
  path (Git _ r) = r

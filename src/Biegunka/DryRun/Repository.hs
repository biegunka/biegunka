module Biegunka.DryRun.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, void)
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, doesFileExist, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))

import Biegunka.Core

data Git = Git { url ∷ FilePath, repo ∷ FilePath }

git ∷ FilePath → FilePath → IO Git
git u p = clone r >>= flip unless (void $ update r) >> return r
  where r = Git u p

instance Repository Git where
  clone (Git u r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    if exists
      then putStrLn ("Something does exist at " ++ r) >> return False
      else putStrLn ("Clone from git at " ++ u ++ " to " ++ r) >> return True
  update (Git _ r) = putStrLn ("Pull from git (check that repo in " ++ r ++ " has remote origin with master branch!)") >> return True
  path = repo

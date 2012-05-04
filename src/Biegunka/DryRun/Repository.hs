module Biegunka.DryRun.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, void)
import System.Directory (doesDirectoryExist, doesFileExist)

import Biegunka.Core

data Git = Git FilePath

git ∷ FilePath → FilePath → IO Git
git _ p = clone r >>= flip unless (void $ update r) >> return r
  where r = Git p

instance Repository Git where
  clone (Git r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    if exists
      then putStrLn ("Something does exist at " ++ r) >> return False
      else putStrLn ("Clone from git to " ++ r) >> return True
  update (Git r) = putStrLn ("Pull from git (check that repo in " ++ r ++ " has remote origin with master branch!)") >> return True
  path (Git r) = r

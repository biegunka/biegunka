-- | Biegunka.Repository module exports a bunch of functions to mimic Biegunka.Repository behaviour and print debug information.
module Biegunka.DryRun.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.Directory (doesDirectoryExist, doesFileExist)

import Biegunka.Core

data Git = Git FilePath

-- | Mimic 'Biegunka.Repository.git' behaviour
git ∷ FilePath → FilePath → IO Git
git _ p = update r >> return r
  where r = Git p

instance Repository Git where
  update (Git r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    unless exists $
      putStrLn ("Clone from git to " ++  r)
    putStrLn ("Pull from git (check that repo in " ++ r ++ " has remote origin with master branch!)")
  path (Git r) = r

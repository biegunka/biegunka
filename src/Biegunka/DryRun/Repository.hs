-- | Biegunka.Repository module exports a bunch of functions to mimic Biegunka.Repository behaviour and print debug information.
module Biegunka.DryRun.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.Directory (doesDirectoryExist, doesFileExist)

import Biegunka.Core

newtype Git = Git { ρ ∷ FilePath }
type UrlPath = String

-- | Mimic 'Biegunka.Repository.git' behaviour
git ∷ UrlPath → FilePath → IO Git
git u p = update u p >> return (Git p)

update ∷ UrlPath → FilePath → IO ()
update u p = do
  exists ← (||) <$> doesDirectoryExist p <*> doesFileExist p
  unless exists $
    putStrLn ("Clone from git to " ++ p)
  putStrLn ("Pull from " ++ u ++ " (check that repo in " ++ p ++ " has remote origin with master branch!)")

instance Repository Git where
  path = ρ

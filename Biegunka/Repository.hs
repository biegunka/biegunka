module Biegunka.Repository
  ( Repository(..)
  , git
  ) where

import Control.Applicative ((<$>), (<*>))
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, doesFileExist, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))

class Repository a where
  clone ∷ a → IO Bool
  update ∷ a → IO Bool
  path ∷ a → String

type UrlPath = String
data Git = Git { url ∷ String, repo ∷ FilePath }

git ∷ UrlPath → FilePath → Git
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

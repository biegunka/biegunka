-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.IO (IOMode(WriteMode), withFile)
import System.Process (runProcess, waitForProcess)
import System.Directory (doesDirectoryExist, doesFileExist)
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
git u p = do
  let r = Git u p
  cloned ← clone r
  unless cloned $ update r
  return r

instance Repository Git where
  clone (Git u r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    if exists
      then return False
      else do
        putStr $ concat ["Clone git repository from ", u, " to ", r, ".. "]
        status ← withFile "/dev/null" WriteMode $ \h →
          waitForProcess =<< runProcess "git" ["clone", u, r] Nothing Nothing Nothing (Just h) (Just h)
        if status == ExitSuccess
          then do
            putStrLn "OK!"
            return True
          else do
            putStrLn "Fail!"
            error $ concat ["git clone ", u, " ", r, " has failed!"]
  update (Git _ r) = do
    putStr $ concat ["Pulling in ", r, " from origin master.. "]
    status ← withFile "/dev/null" WriteMode $ \h →
      waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just r) Nothing Nothing (Just h) (Just h)
    if status == ExitSuccess
      then putStrLn "OK!"
      else do
        putStrLn "Fail!"
        error $ concat ["cd ", r, "; git pull origin master has failed!"]
  path (Git _ r) = r

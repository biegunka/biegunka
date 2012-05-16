-- | Biegunka.Repository module exports a bunch of functions to connect scripts with various VCS instances.
module Biegunka.Repository
  ( git
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.IO (Handle, IOMode(WriteMode), hFlush, stdout, withFile)
import System.Process (runProcess, waitForProcess)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(..))

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
  update r
  return r

instance Repository Git where
  update (Git u r) = do
    exists ← (||) <$> doesDirectoryExist r <*> doesFileExist r
    unless exists $
      withProgressString ("Clone git repository from " ++ u ++ " to " ++ r ++ "… ") $
        withTempFile $ \h →
          waitForProcess =<< runProcess "git" ["clone", u, r] Nothing Nothing Nothing (Just h) (Just h)
    withProgressString ("Pulling in " ++ r ++ " from origin master… ") $
      withTempFile $ \h →
        waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just r) Nothing Nothing (Just h) (Just h)
  path (Git _ r) = r

withProgressString ∷ String → IO ExitCode → IO ()
withProgressString hello μ = do
  putStr hello >> hFlush stdout
  result ← μ
  printResult result
  where printResult ∷ ExitCode → IO ()
        printResult ExitSuccess = putStrLn "OK!"
        printResult (ExitFailure _) = do
          putStrLn "Fail!"
          errors ← readFile "/tmp/biegunka.errors"
          error errors

withTempFile ∷ (Handle → IO α) → IO α
withTempFile = withFile "/tmp/biegunka.errors" WriteMode

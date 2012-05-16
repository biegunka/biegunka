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

newtype Git = Git { ρ ∷ FilePath }
type UrlPath = String

instance Repository Git where
  path = ρ

-- | Setup git instance as follows:
--
-- 1. If specified directory doesn't exist then clone repository from specified URL to it.
--
-- 2. Pull from origin master.
--
-- 3. Return ADT with specified repository root path.
git ∷ UrlPath → FilePath → IO Git
git u p = do
  update u p
  return (Git p)

update ∷ UrlPath → FilePath → IO ()
update u p = do
  exists ← (||) <$> doesDirectoryExist p <*> doesFileExist p
  unless exists $
    withProgressString ("Clone git repository from " ++ u ++ " to " ++ p ++ "… ") $
      withTempFile $ \h →
        waitForProcess =<< runProcess "git" ["clone", u, p] Nothing Nothing Nothing (Just h) (Just h)
  withProgressString ("Pulling in " ++ p ++ " from origin master… ") $
    withTempFile $ \h →
      waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just p) Nothing Nothing (Just h) (Just h)

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

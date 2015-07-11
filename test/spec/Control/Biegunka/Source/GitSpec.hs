module Control.Biegunka.Source.GitSpec (spec) where

import           Control.Applicative (liftA2)
import           Control.Lens (set)
import           Data.Maybe (listToMaybe)
import           System.Directory (createDirectory)
import           System.FilePath ((</>))
import           Test.Hspec.Lens
import           Text.Read (readMaybe)

import           Control.Biegunka
import           Control.Biegunka.Execute.Exception (_SourceException)
import           Control.Biegunka.Source (Url, url)
import qualified Control.Biegunka.Source.Git.Internal as Git
import           SpecHelper (withBiegunkaTempDirectory)

{-# ANN module "HLint: ignore Reduce duplication" #-}


spec :: Spec
spec =
  around withBiegunkaTempDirectory $ do
    describe "updateGit" $ do
      context "when local path doesn't exist" $
        it "creates new directory and sets branch correctly" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          fullUpdate (url repoRemote . path repoLocal)
          currentBranch repoLocal `shouldReturn` Just "master"
          modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has no commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $
          it "does not change checkout state" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.runGit tmp ["clone", repoRemote, repoLocal]
            gitHashBefore <- Git.gitHash repoLocal "HEAD"
            fullUpdate (url repoRemote . path repoLocal)
            gitHashAfter <- Git.gitHash repoLocal "HEAD"
            gitHashAfter `shouldBe` gitHashBefore
            modifiedFiles repoLocal `shouldReturn` []

        context "when remote branch has new commits" $
          it "updates local branch" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.runGit tmp ["clone", repoRemote, repoLocal]
            gitHashOfNewCommit <- Git.gitHash repoLocal "HEAD"
            Git.runGit repoLocal ["reset", "--hard", "HEAD~1"]
            fullUpdate (url repoRemote . path repoLocal)
            gitHashAfter <- Git.gitHash repoLocal "HEAD"
            gitHashAfter `shouldBe` gitHashOfNewCommit
            modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "does not change checkout state" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.runGit tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              gitHashBefore <- Git.gitHash repoLocal "HEAD"
              fullUpdate (url repoRemote . path repoLocal)
              gitHashAfter <- Git.gitHash repoLocal "HEAD"
              gitHashAfter `shouldBe` gitHashBefore
              modifiedFiles repoLocal `shouldReturn` []

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.runGit tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              fullUpdate (url repoRemote . path repoLocal . Git.failIfAhead) `shouldThrow` _SourceException

        context "when remote branch has new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "rebases local commits onto a remote branch" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.runGit tmp ["clone", repoRemote, repoLocal]
              numberOfCommitsRemote <- numberOfCommits repoLocal
              Git.runGit repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              fullUpdate (url repoRemote . path repoLocal)
              numberOfCommitsLocal <- numberOfCommits repoLocal
              modifiedFiles repoLocal `shouldReturn` []
              liftA2 (-) numberOfCommitsLocal numberOfCommitsRemote `shouldBe` Just 1

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.runGit tmp ["clone", repoRemote, repoLocal]
              Git.runGit repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              fullUpdate (url repoRemote . path repoLocal . Git.failIfAhead) `shouldThrow` _SourceException

      context "when repo has a dirty state" $
        it "fails with exception" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.runGit tmp ["clone", repoRemote, repoLocal]
          Git.runGit repoLocal ["reset", "--soft", "HEAD~1"]
          fullUpdate (url repoRemote . path repoLocal) `shouldThrow` _SourceException

      context "when current branch differs from the one biegunka going to checkout" $
        it "fails with exception" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.runGit tmp ["clone", repoRemote, repoLocal]
          Git.runGit repoLocal ["checkout", "-b", "another-branch"]
          fullUpdate (url repoRemote . path repoLocal) `shouldThrow` _SourceException

      context "when remote uri from a local repo differs from the one biegunka going to fetch from" $
        it "fails with exception" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.runGit tmp ["clone", repoRemote, repoLocal]
          Git.runGit repoLocal ["remote", "set-url", "origin", "https://example.com"]
          fullUpdate (url repoRemote . path repoLocal) `shouldThrow` _SourceException

    describe "biegunka" $
      context "when local branch has no commits ahead of remote branch" $
        context "when remote branch has new commits" $
          it "updates local branch" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.runGit tmp ["clone", repoRemote, repoLocal]
            gitHashOfNewCommit <- Git.gitHash repoLocal "HEAD"
            Git.runGit repoLocal ["reset", "--hard", "HEAD~1"]
            biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
              Git.git (url repoRemote . path repoLocal) pass
            gitHashAfter <- Git.gitHash repoLocal "HEAD"
            gitHashAfter `shouldBe` gitHashOfNewCommit
            modifiedFiles repoLocal `shouldReturn` []


fullUpdate :: Git.Git Url FilePath -> IO ()
fullUpdate f = () <$ do
  (_, finish) <- Git.update config (Git.configPath config)
  finish
 where
  config = f Git.defaultConfig

currentBranch :: FilePath -> IO (Maybe String)
currentBranch fp = listToMaybe . lines <$> Git.runGit fp ["rev-parse", "--abbrev-ref", "HEAD"]

modifiedFiles :: FilePath -> IO [String]
modifiedFiles fp = lines <$> Git.runGit fp ["diff-index", "HEAD"]

buildRemoteRepo :: FilePath -> IO (FilePath, FilePath)
buildRemoteRepo fp = do
  let repo = fp </> "test-repo-remote"
  let file = repo </> "file"
  createDirectory repo
  Git.runGit repo ["init"]
  setCredentials repo
  Git.runGit repo ["commit", "--allow-empty", "--message", "initial"]
  writeFile file "hi\nthere\n!\n"
  Git.runGit repo ["add", file]
  Git.runGit repo ["commit", "--message", "Change 1"]
  appendFile file "never\nmind\n!\n"
  Git.runGit repo ["commit", "-a", "--message", "Change 2"]
  appendFile file "just\nkidding\n!\n"
  Git.runGit repo ["commit", "-a", "--message", "Change 3"]
  return (repo, fp </> "test-repo-local")

shouldReturn :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn mx y = do
  x <- mx
  x `shouldBe` y

addNewFile :: FilePath -> IO ()
addNewFile fp = () <$ do
  setCredentials fp
  let anotherFile = fp </> "another-file"
  writeFile anotherFile "something"
  Git.runGit fp ["add", anotherFile]
  Git.runGit fp ["commit", "-m", "add new file"]

numberOfCommits :: FilePath -> IO (Maybe Int)
numberOfCommits fp = (readMaybe =<<) . listToMaybe . lines <$> Git.runGit fp ["rev-list", "--count", "HEAD"]

setCredentials :: FilePath -> IO ()
setCredentials fp = () <$ do
  Git.runGit fp ["config", "--local", "user.name", "A U Thor"]
  Git.runGit fp ["config", "--local", "user.email", "author@example.com"]

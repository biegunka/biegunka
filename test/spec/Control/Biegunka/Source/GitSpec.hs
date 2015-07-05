module Control.Biegunka.Source.GitSpec (spec) where

import           Control.Applicative                  (liftA2)
import           Data.Maybe                           (listToMaybe)
import           System.Directory                     (createDirectory)
import           System.FilePath                      ((</>))
import           System.IO.Temp                       (withSystemTempDirectory)
import           Test.Hspec.Lens
import           Text.Read                            (readMaybe)

import           Control.Biegunka.Execute.Exception   (_SourceException)
import qualified Control.Biegunka.Source.Git.Internal as Git

{-# ANN module "HLint: ignore Reduce duplication" #-}


spec :: Spec
spec =
  around (withSystemTempDirectory "biegunka-") $
    describe "updateGit" $ do
      context "when local path doesn't exist" $
        it "creates new directory and sets branch correctly" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          fullUpdate repoRemote repoLocal Git.defaultGit
          currentBranch repoLocal `shouldReturn` Just "master"
          modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has no commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $
          it "does not change checkout state" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.askGit_ tmp ["clone", repoRemote, repoLocal]
            gitHashBefore <- Git.gitHash repoLocal "HEAD"
            fullUpdate repoRemote repoLocal Git.defaultGit
            gitHashAfter <- Git.gitHash repoLocal "HEAD"
            gitHashAfter `shouldBe` gitHashBefore
            modifiedFiles repoLocal `shouldReturn` []

        context "when remote branch has new commits" $
          it "updates local branch" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.askGit_ tmp ["clone", repoRemote, repoLocal]
            gitHashOfNewCommit <- Git.gitHash repoLocal "HEAD"
            Git.askGit_ repoLocal ["reset", "--hard", "HEAD~1"]
            fullUpdate repoRemote repoLocal Git.defaultGit
            gitHashAfter <- Git.gitHash repoLocal "HEAD"
            gitHashAfter `shouldBe` gitHashOfNewCommit
            modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "does not change checkout state" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit_ tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              gitHashBefore <- Git.gitHash repoLocal "HEAD"
              fullUpdate repoRemote repoLocal Git.defaultGit
              gitHashAfter <- Git.gitHash repoLocal "HEAD"
              gitHashAfter `shouldBe` gitHashBefore
              modifiedFiles repoLocal `shouldReturn` []

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit_ tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              fullUpdate repoRemote repoLocal (Git.defaultGit { Git._failIfAhead = True })
                `shouldThrow` _SourceException

        context "when remote branch has new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "rebases local commits onto a remote branch" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit_ tmp ["clone", repoRemote, repoLocal]
              numberOfCommitsRemote <- numberOfCommits repoLocal
              Git.askGit_ repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              fullUpdate repoRemote repoLocal Git.defaultGit
              numberOfCommitsLocal <- numberOfCommits repoLocal
              modifiedFiles repoLocal `shouldReturn` []
              liftA2 (-) numberOfCommitsLocal numberOfCommitsRemote `shouldBe` Just 1

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit_ tmp ["clone", repoRemote, repoLocal]
              Git.askGit_ repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              fullUpdate repoRemote repoLocal (Git.defaultGit { Git._failIfAhead = True })
                `shouldThrow` _SourceException

      context "when repo has a dirty state" $
        it "fails with exception" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.askGit_ tmp ["clone", repoRemote, repoLocal]
          Git.askGit_ repoLocal ["reset", "--soft", "HEAD~1"]
          fullUpdate repoRemote repoLocal Git.defaultGit `shouldThrow` _SourceException

fullUpdate :: Git.URI -> FilePath -> Git.Git -> IO ()
fullUpdate url fp config = do
  (_, finish) <- Git.updateGit url fp config
  finish

currentBranch :: FilePath -> IO (Maybe String)
currentBranch path = listToMaybe . lines <$> Git.askGit path ["rev-parse", "--abbrev-ref", "HEAD"]

modifiedFiles :: FilePath -> IO [String]
modifiedFiles path = lines <$> Git.askGit path ["diff-index", "HEAD"]

buildRemoteRepo :: FilePath -> IO (FilePath, FilePath)
buildRemoteRepo path = do
  let repo = path </> "test-repo-remote"
  let file = repo </> "file"
  createDirectory repo
  Git.askGit_ repo ["init"]
  setCredentials repo
  Git.askGit_ repo ["commit", "--allow-empty", "--message", "initial"]
  writeFile file "hi\nthere\n!\n"
  Git.askGit_ repo ["add", file]
  Git.askGit_ repo ["commit", "--message", "Change 1"]
  appendFile file "never\nmind\n!\n"
  Git.askGit_ repo ["commit", "-a", "--message", "Change 2"]
  appendFile file "just\nkidding\n!\n"
  Git.askGit_ repo ["commit", "-a", "--message", "Change 3"]
  return (repo, path </> "test-repo-local")

shouldReturn :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn mx y = do
  x <- mx
  x `shouldBe` y

addNewFile :: FilePath -> IO ()
addNewFile path = do
  setCredentials path
  let anotherFile = path </> "another-file"
  writeFile anotherFile "something"
  Git.askGit_ path ["add", anotherFile]
  Git.askGit_ path ["commit", "-m", "add new file"]

numberOfCommits :: FilePath -> IO (Maybe Int)
numberOfCommits path = (readMaybe =<<) . listToMaybe . lines <$> Git.askGit path ["rev-list", "--count", "HEAD"]

setCredentials :: FilePath -> IO ()
setCredentials path = do
  Git.askGit_ path ["config", "--local", "user.name", "A U Thor"]
  Git.askGit_ path ["config", "--local", "user.email", "author@example.com"]

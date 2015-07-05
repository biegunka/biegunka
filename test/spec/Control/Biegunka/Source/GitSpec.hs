module Control.Biegunka.Source.GitSpec (spec) where
import           Test.Hspec

import           Control.Biegunka.Execute.Exception   (SourceException (..))
import qualified Control.Biegunka.Source.Git.Internal as Git

import           Control.Applicative                  (liftA2)
import           Data.Maybe                           (listToMaybe)
import           System.Directory                     (createDirectory)
import           System.FilePath                      ((</>))
import           System.IO.Temp                       (withSystemTempDirectory)
import           Text.Read                            (readMaybe)

spec :: Spec
spec = do
  around (withSystemTempDirectory "biegunka-") $
    describe "updateGit" $ do
      context "when local path doesn't exist" $
        it "creates new directory and sets branch correctly" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.updateGit repoRemote repoLocal Git.defaultGit
          currentBranch repoLocal `shouldReturn` Just "master"
          modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has no commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $
          it "does not change checkout state" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.askGit' tmp ["clone", repoRemote, repoLocal]
            gitHashBefore <- Git.gitHash repoLocal
            Git.updateGit repoRemote repoLocal Git.defaultGit
            gitHashAfter <- Git.gitHash repoLocal
            gitHashAfter `shouldBe` gitHashBefore
            modifiedFiles repoLocal `shouldReturn` []

        context "when remote branch has new commits" $
          it "updates local branch" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.askGit' tmp ["clone", repoRemote, repoLocal]
            gitHashOfNewCommit <- Git.gitHash repoLocal
            Git.askGit' repoLocal ["reset", "--hard", "HEAD~1"]
            Git.updateGit repoRemote repoLocal Git.defaultGit
            gitHashAfter <- Git.gitHash repoLocal
            gitHashAfter `shouldBe` gitHashOfNewCommit
            modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "does not change checkout state" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit' tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              gitHashBefore <- Git.gitHash repoLocal
              Git.updateGit repoRemote repoLocal Git.defaultGit
              gitHashAfter <- Git.gitHash repoLocal
              gitHashAfter `shouldBe` gitHashBefore
              modifiedFiles repoLocal `shouldReturn` []

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit' tmp ["clone", repoRemote, repoLocal]
              addNewFile repoLocal
              Git.updateGit repoRemote repoLocal (Git.defaultGit { Git._failIfAhead = True })
                `shouldThrow` sourceException

        context "when remote branch has new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "rebases local commits onto a remote branch" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit' tmp ["clone", repoRemote, repoLocal]
              numberOfCommitsRemote <- numberOfCommits repoLocal
              Git.askGit' repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              Git.updateGit repoRemote repoLocal Git.defaultGit
              numberOfCommitsLocal <- numberOfCommits repoLocal
              modifiedFiles repoLocal `shouldReturn` []
              liftA2 (-) numberOfCommitsLocal numberOfCommitsRemote `shouldBe` Just 1

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit' tmp ["clone", repoRemote, repoLocal]
              Git.askGit' repoLocal ["reset", "--hard", "HEAD~1"]
              addNewFile repoLocal
              Git.updateGit repoRemote repoLocal (Git.defaultGit { Git._failIfAhead = True })
                `shouldThrow` sourceException

      context "when repo has a dirty state" $
        it "fails with exception" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.askGit' tmp ["clone", repoRemote, repoLocal]
          Git.askGit' repoLocal ["reset", "--soft", "HEAD~1"]
          Git.updateGit repoRemote repoLocal Git.defaultGit `shouldThrow` sourceException


currentBranch :: FilePath -> IO (Maybe String)
currentBranch path = listToMaybe . lines <$> Git.askGit path ["rev-parse", "--abbrev-ref", "HEAD"]

modifiedFiles :: FilePath -> IO [String]
modifiedFiles path = lines <$> Git.askGit path ["diff-index", "HEAD"]

buildRemoteRepo :: FilePath -> IO (FilePath, FilePath)
buildRemoteRepo path = do
  let repo = path </> "test-repo-remote"
  let file = repo </> "file"
  createDirectory repo
  Git.askGit' repo ["init"]
  setCredentials repo
  Git.askGit' repo ["commit", "--allow-empty", "--message", "initial"]
  writeFile file "hi\nthere\n!\n"
  Git.askGit' repo ["add", file]
  Git.askGit' repo ["commit", "--message", "Change 1"]
  appendFile file "never\nmind\n!\n"
  Git.askGit' repo ["commit", "-a", "--message", "Change 2"]
  appendFile file "just\nkidding\n!\n"
  Git.askGit' repo ["commit", "-a", "--message", "Change 3"]
  return (repo, path </> "test-repo-local")

sourceException :: Selector SourceException
sourceException = const True

addNewFile :: FilePath -> IO ()
addNewFile path = do
  setCredentials path
  let anotherFile = path </> "another-file"
  writeFile anotherFile "something"
  Git.askGit' path ["add", anotherFile]
  Git.askGit' path ["commit", "-m", "add new file"]

numberOfCommits :: FilePath -> IO (Maybe Int)
numberOfCommits path = (readMaybe =<<) . listToMaybe . lines <$> Git.askGit path ["rev-list", "--count", "HEAD"]

setCredentials :: FilePath -> IO ()
setCredentials path = do
  Git.askGit' path ["config", "--local", "user.name", "A U Thor"]
  Git.askGit' path ["config", "--local", "user.email", "author@example.com"]

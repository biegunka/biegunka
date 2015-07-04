module Control.Biegunka.Source.GitSpec (spec) where
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

import qualified Control.Biegunka.Source.Git.Internal as Git

import           Control.Monad                        (liftM)
import           Data.Maybe                           (listToMaybe)
import           System.Directory                     (createDirectory)
import           System.FilePath                      ((</>))
import           System.IO.Temp                       (withSystemTempDirectory)
import           System.Random

spec :: Spec
spec = do
  around withBiegunkaDirectory $
    describe "updateGit" $ do
      context "when local path doesn't exist" $
        it "creates new directory and sets branch correctly" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          Git.updateGit repoRemote repoLocal Git.defaultGit
          currentBranch repoLocal `shouldReturn` Just "master"
          modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has no commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $
          it "does nothing" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            Git.askGit' tmp ["clone", repoRemote, repoLocal]
            gitHashBefore <- Git.gitHash repoLocal
            Git.updateGit repoRemote repoLocal Git.defaultGit
            gitHashAfter <- Git.gitHash repoLocal
            gitHashBefore `shouldBe` gitHashAfter
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
            it "does nothing" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              Git.askGit' tmp ["clone", repoRemote, repoLocal]
              gitHashBefore <- Git.gitHash repoLocal
              appendFile (repoLocal </> "file") "new string"
              Git.askGit repoLocal $ credentials ++ ["commit", "-a", "-m", "add new string to file"]
              Git.updateGit repoRemote repoLocal Git.defaultGit
              gitHashAfter <- Git.gitHash repoLocal
              modifiedFiles repoLocal `shouldReturn` []
              gitHashAfter `shouldNotBe` gitHashBefore

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \_ ->
              pending

        context "when remote branch has new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "rebases local commits onto a remote branch" $ \_ ->
              pending

          context "when failIfAhead flag is set" $
            it "fails with exception" $ \_ ->
              pending

      context "when repo has a dirty state" $
        it "fails with exception" $ \_ ->
          pending

withBiegunkaDirectory :: (FilePath -> IO a) -> IO a
withBiegunkaDirectory action = do
 str <- liftM (take 10 . randomRs ('a','z')) newStdGen
 withSystemTempDirectory ("biegunka-" ++ str ++ "-") action

currentBranch :: FilePath -> IO (Maybe String)
currentBranch path = (listToMaybe . lines) `fmap` Git.askGit path ["rev-parse", "--abbrev-ref", "HEAD"]

modifiedFiles :: FilePath -> IO [String]
modifiedFiles repo = lines `fmap` Git.askGit repo ["diff-index", "HEAD"]

buildRemoteRepo :: FilePath -> IO (FilePath, FilePath)
buildRemoteRepo path = do
  let repo = path </> "test-repo-remote"
  let file = repo </> "file"
  createDirectory repo
  Git.askGit' repo ["init"]
  Git.askGit' repo $ credentials ++ ["commit", "--allow-empty", "--message", "initial"]
  writeFile file "hi\nthere\n!\n"
  Git.askGit' repo ["add", file]
  Git.askGit' repo $ credentials ++ ["commit", "--message", "Change 1"]
  writeFile file "never\nmind\n!\n"
  Git.askGit' repo $ credentials ++ ["commit", "-a", "--message", "Change 2"]
  writeFile file "just\nkidding\n!\n"
  Git.askGit' repo $ credentials ++ ["commit", "-a", "--message", "Change 3"]
  return (repo, path </> "test-repo-local")

credentials :: [String]
credentials = ["-c", "user.name=biegunka-test", "-c", "user.email=mail@example.com"]

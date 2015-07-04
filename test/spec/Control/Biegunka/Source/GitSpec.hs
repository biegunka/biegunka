module Control.Biegunka.Source.GitSpec (spec) where
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

import           Control.Biegunka
import           Control.Biegunka.Execute.Exception (onFailure, sourceFailure)
import           Control.Biegunka.Source.Git        (git_)

import           Control.Lens
import           Control.Monad                      (liftM)
import           Control.Monad                      (void)
import           Data.Maybe                         (listToMaybe)
import qualified Data.Text                          as Text
import           System.Directory                   (createDirectory)
import           System.FilePath                    ((</>))
import           System.IO.Silently                 (silence)
import           System.IO.Temp                     (withSystemTempDirectory)
import qualified System.Process                     as P
import           System.Random

spec :: Spec
spec = do
  around withBiegunkaDirectory $
    describe "git" $ do
      context "when local path doesn't exist" $
        it "creates new directory and sets branch correctly" $ \tmp -> do
          (repoRemote, repoLocal) <- buildRemoteRepo tmp
          silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
            retries 0 $ git_ repoRemote repoLocal
          currentBranch repoLocal `shouldReturn` Just "master"
          modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has no commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $
          it "does nothing" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            askGit' tmp ["clone", repoRemote, repoLocal]
            gitHashBefore <- gitHash repoLocal
            silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
              retries 0 $ git_ repoRemote repoLocal
            gitHashAfter <- gitHash repoLocal
            gitHashBefore `shouldBe` gitHashAfter
            modifiedFiles repoLocal `shouldReturn` []

        context "when remote branch has new commits" $
          it "updates local branch" $ \tmp -> do
            (repoRemote, repoLocal) <- buildRemoteRepo tmp
            askGit' tmp ["clone", repoRemote, repoLocal]
            gitHashOfNewCommit <- gitHash repoLocal
            askGit' repoLocal ["reset", "--hard", "HEAD~1"]
            silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
              retries 0 $ git_ repoRemote repoLocal
            gitHashAfter <- gitHash repoLocal
            gitHashAfter `shouldBe` gitHashOfNewCommit
            modifiedFiles repoLocal `shouldReturn` []

      context "when local branch has commits ahead of remote branch" $ do
        context "when remote branch has no new commits" $ do
          context "when failIfAhead flag isn't set" $
            it "does nothing" $ \tmp -> do
              (repoRemote, repoLocal) <- buildRemoteRepo tmp
              askGit' tmp ["clone", repoRemote, repoLocal]
              gitHashBefore <- gitHash repoLocal
              appendFile (repoLocal </> "file") "new string"
              askGit repoLocal $ credentials ++ ["commit", "-a", "-m", "add new string to file"]
              silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
                retries 0 $ git_ repoRemote repoLocal
              gitHashAfter <- gitHash repoLocal
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
currentBranch path = (listToMaybe . lines) `fmap` askGit path ["rev-parse", "--abbrev-ref", "HEAD"]

modifiedFiles :: FilePath -> IO [String]
modifiedFiles repo = lines `fmap` askGit repo ["diff-index", "HEAD"]

gitHash :: FilePath -> IO String
gitHash path = askGit path ["rev-parse", "--short", "HEAD"]

askGit :: FilePath -> [String] -> IO String
askGit cwd args = Text.unpack . Text.stripEnd <$> go
 where
  go = do
    let proc = P.proc "git" args
    (exitcode, out, err) <-
      P.readCreateProcessWithExitCode proc { P.cwd = Just cwd } ""
    exitcode `onFailure` \_ -> sourceFailure (Text.pack err)
    return (Text.pack out)

askGit' :: FilePath -> [String] -> IO ()
askGit' f args = void $ askGit f args

buildRemoteRepo :: FilePath -> IO (FilePath, FilePath)
buildRemoteRepo path = do
  let repo = path </> "test-repo-remote"
  let file = repo </> "file"
  createDirectory repo
  askGit' repo ["init"]
  askGit' repo $ credentials ++ ["commit", "--allow-empty", "--message", "initial"]
  writeFile file "hi\nthere\n!\n"
  askGit' repo ["add", file]
  askGit' repo $ credentials ++ ["commit", "--message", "Change 1"]
  writeFile file "never\nmind\n!\n"
  askGit' repo $ credentials ++ ["commit", "-a", "--message", "Change 2"]
  writeFile file "just\nkidding\n!\n"
  askGit' repo $ credentials ++ ["commit", "-a", "--message", "Change 3"]
  return (repo, path </> "test-repo-local")

credentials :: [String]
credentials = ["-c", "user.name=biegunka-test", "-c", "user.email=mail@example.com"]

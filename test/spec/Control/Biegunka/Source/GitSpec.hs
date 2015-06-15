module Control.Biegunka.Source.GitSpec (spec) where
import           Test.Hspec

import           Control.Biegunka
import           Control.Biegunka.Source.Git (git_)
import           Control.Lens
import           Data.Maybe                  (listToMaybe)
import           System.FilePath             ((</>))
import           System.IO.Silently          (silence)
import           System.IO.Temp              (withSystemTempDirectory)
import qualified System.Process              as P

spec :: Spec
spec = do
  describe "git_" $ around withBiegunkaDirectory $ do

    context "when local path doesn't exist" $

      it "creates new directory and set branch correctly" $ \tmp -> do
          let repo = tmp </> "biegunka-test"
          silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
            git_ testRepo "biegunka-test"
          currentBranch repo `shouldReturn` Just "master"
          repoHaveCleanState repo `shouldReturn` True

    context "when local branch have no commits ahead of remote branch" $ do

      context "when remote branch have no new commits" $

        it "does nothing" $ \tmp -> do
            let repo = tmp </> "biegunka-test"
            _ <- askGit tmp ["clone", testRepo]
            gitHashBefore <- gitHash repo
            silence $ biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
              git_ testRepo "biegunka-test"
            gitHashAfter <- gitHash repo
            gitHashBefore `shouldBe` gitHashAfter
            repoHaveCleanState repo `shouldReturn` True

      context "when remote branch have new commits" $

        it "updates local branch" $ \_ ->
          pending

    context "when local branch have commits ahead of remote branch" $ do

      context "when remote branch have no new commits" $ do

        context "when failIfAhead flag isn't set" $

          it "does nothing" $ \_ ->
            pending

        context "when failIfAhead flag is set" $

          it "fails with exception" $ \_ ->
            pending

      context "when remote branch have new commits" $ do

        context "when failIfAhead flag isn't set" $

          it "rebases local commits onto a remote branch" $ \_ ->
            pending

        context "when failIfAhead flag is set" $
          it "fails with exception" $ \_ ->
            pending

    context "when repo have a dirty state" $
      it "fails with exception" $ \_ ->
        pending


withBiegunkaDirectory :: (FilePath -> IO a) -> IO a
withBiegunkaDirectory = withSystemTempDirectory "biegunka-"

currentBranch :: FilePath -> IO (Maybe String)
currentBranch path = (listToMaybe . lines) `fmap` askGit path ["rev-parse", "--abbrev-ref", "HEAD"]

repoHaveCleanState :: FilePath -> IO Bool
repoHaveCleanState path = (null . lines) `fmap` askGit path ["diff-index", "HEAD"]

gitHash :: FilePath -> IO String
gitHash path = askGit path ["rev-parse", "--short", "HEAD"]

askGit :: FilePath -> [String] -> IO String
askGit cwd args = do
  let proc = P.proc "git" args
  (_, out, _) <-
    P.readCreateProcessWithExitCode proc { P.cwd = Just cwd } ""
  return out

testRepo :: String
testRepo = "https://bitbucket.com/dmalikov/biegunka-test"

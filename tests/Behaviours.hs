{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Biegunka hiding (check)
import           Biegunka.Source.Layout (layout)
import qualified Biegunka.Source.Directory as D
import           Control.Lens
import           System.Directory.Layout
import           Test.Hspec


main :: IO ()
main = do
  hspec $ do
    describe "Trivial biegunka script" $ do
      it "should be trivial layout too" $ do
        xs <- trivial_script `resultsIn` trivial_layout
        null xs `shouldBe` True
    describe "Trivial biegunka profile script" $ do
      it "should be trivial layout too" $ do
        xs <- trivial_repo "biegunka-core-test" `resultsIn` trivial_layout
        null xs `shouldBe` True
    describe "Simple biegunka profile script" $ do
      it "should be simple layout too" $ do
        xs <- simple_repo_0 `resultsIn` simple_layout_0
        null xs `shouldBe` True
      it "should disappear after deletion" $ do
        xs <- trivial_repo "biegunka-core-simple0" `resultsIn` trivial_layout
        null xs `shouldBe` True
    describe "Simple biegunka no profile script" $ do
      it "should be simple layout too" $ do
        xs <- simple_repo_no_profile_0 `resultsIn` simple_layout_no_profile_0
        null xs `shouldBe` True
      it "should disappear after deletion" $ do
        xs <- trivial_repo "" `resultsIn` trivial_layout
        null xs `shouldBe` True
    describe "Simple copying" $ do
      it "should copy the directory correctly" $ do
        make (directory "a" simple_copying_layout_0) "/tmp"
        biegunka (set root "/tmp" . set appData "/tmp/.biegunka") (run id) $
          D.directory "/tmp" $
            copy "a" "/tmp/b"
        check (directory "b" simple_copying_layout_0) "/tmp" `shouldReturn` []
      it "should disappear after deletion" $ do
        xs <- trivial_repo "" `resultsIn` trivial_layout
        null xs `shouldBe` True


resultsIn :: Script Sources () -> Layout -> IO [LayoutException]
resultsIn s l = do
  biegunka (set root "/tmp" . set appData "/tmp/.biegunka") (run id) s
  check l "/tmp"


trivial_script :: Script Sources ()
trivial_script = return ()


trivial_layout :: Layout
trivial_layout = return ()


trivial_repo :: String -> Script Sources ()
trivial_repo p = profile p $ return ()


simple_repo_0 :: Script Sources ()
simple_repo_0 =
  profile "biegunka-core-simple0" $
    layout l "tmp/biegunka-core-simple0" $
      copy "src0" "tmp/dst0"
 where
  l = file "src0" "thisiscontents\n"

simple_layout_0 :: Layout
simple_layout_0 = do
  directory "tmp" $
    file "dst0" "thisiscontents\n"
  directory ".biegunka" $
    directory "profiles" $
      file_ "biegunka-core-simple0.profile"

simple_repo_no_profile_0 :: Script Sources ()
simple_repo_no_profile_0 =
  layout l "tmp/biegunka-core-simple0" $
    copy "src0" "tmp/dst0"
 where
  l = file "src0" "thisiscontents\n"

simple_layout_no_profile_0 :: Layout
simple_layout_no_profile_0 = do
  directory "tmp" $
    file "dst0" "thisiscontents\n"
  directory ".biegunka" $
    directory "profiles" $
      file_ ".profile"

simple_copying_layout_0 :: Layout
simple_copying_layout_0 = do
  file "foo" "foocontents\n"
  file "bar" "barcontents\n"
  directory "baz" $
    file "quux" "quuxcontents\n"

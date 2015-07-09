{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module InitSpec (spec) where

import           Control.Lens
import           Prelude hiding (init)
import qualified System.Directory.Layout as Layout
import           System.Exit.Lens (_ExitFailure)
import           System.FilePath (combine)
import qualified System.IO.Temp as IO
import           Test.Hspec.Lens

import           Init (init)


spec :: Spec
spec =
  describe "init" $
    around (IO.withSystemTempDirectory "biegunka") $ do
      context "there's no file at path provided to ‘init’" $
        it "copies the template" $ \tmpDir -> do
          Layout.make tmpDir $
            Layout.dir "data" $
              Layout.file "Template.hs"
                & Layout.contents ?~ "foo"
          init tmpDir (\_ -> return False) (return (combine tmpDir "data/Template.hs"))
          res <- Layout.fit tmpDir $
            Layout.file "Biegunka.hs"
              & Layout.contents ?~ "foo"
              & Layout.mode ?~ 0o100644
          res `shouldHave` _Right

      context "there's a file at path provided to ‘init’" $ do
        context "user refuses to overwrite it" $
          it "leaves the file alone" $ \tmpDir -> do
            Layout.make tmpDir $ do
              Layout.dir "data" $
                Layout.file "Template.hs"
                  & Layout.contents ?~ "bar"
              Layout.file "Biegunka.hs"
                & Layout.contents ?~ "foo"
                & Layout.mode ?~ 0o100644
            init tmpDir (\_ -> return False) (return (combine tmpDir "data/Template.hs"))
              `shouldThrow` _ExitFailure
            res <- Layout.fit tmpDir $
              Layout.file "Biegunka.hs"
                & Layout.contents ?~ "foo"
                & Layout.mode ?~ 0o100644
            res `shouldHave` _Right

        context "user agrees to overwrite it" $
          it "overwrites the file with the template" $ \tmpDir -> do
            Layout.make tmpDir $ do
              Layout.dir "data" $
                Layout.file "Template.hs"
                  & Layout.contents ?~ "bar"
              Layout.file "Biegunka.hs"
                & Layout.contents ?~ "foo"
                & Layout.mode ?~ 0o100644
            init tmpDir (\_ -> return True) (return (combine tmpDir "data/Template.hs"))
            res <- Layout.fit tmpDir $
              Layout.file "Biegunka.hs"
                & Layout.contents ?~ "bar"
                & Layout.mode ?~ 0o100644
            res `shouldHave` _Right

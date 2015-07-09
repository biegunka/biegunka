{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module InitSpec (spec) where

import Control.Lens
import Prelude hiding (init)
import System.Directory.Layout
import System.Exit.Lens (_ExitFailure)
import System.FilePath (combine)
import Test.Hspec.Lens

import Init (init)
import SpecHelper (withBiegunkaTempDirectory)


spec :: Spec
spec =
  describe "init" $
    around withBiegunkaTempDirectory $ do
      context "there's no file at path provided to ‘init’" $
        it "copies the template" $ \tmpDir -> do
          make tmpDir $
            dir "data" $
              file "Template.hs"
                & contents ?~ "foo"
          init tmpDir (\_ -> return False) (return (combine tmpDir "data/Template.hs"))
          res <- fit tmpDir $
            file "Biegunka.hs"
              & contents ?~ "foo"
              & mode ?~ 0o100644
          res `shouldHave` _Right

      context "there's a file at path provided to ‘init’" $ do
        context "user refuses to overwrite it" $
          it "leaves the file alone" $ \tmpDir -> do
            make tmpDir $ do
              dir "data" $
                file "Template.hs"
                  & contents ?~ "bar"
              file "Biegunka.hs"
                & contents ?~ "foo"
                & mode ?~ 0o100644
            init tmpDir (\_ -> return False) (return (combine tmpDir "data/Template.hs"))
              `shouldThrow` _ExitFailure
            res <- fit tmpDir $
              file "Biegunka.hs"
                & contents ?~ "foo"
                & mode ?~ 0o100644
            res `shouldHave` _Right

        context "user agrees to overwrite it" $
          it "overwrites the file with the template" $ \tmpDir -> do
            make tmpDir $ do
              dir "data" $
                file "Template.hs"
                  & contents ?~ "bar"
              file "Biegunka.hs"
                & contents ?~ "foo"
                & mode ?~ 0o100644
            init tmpDir (\_ -> return True) (return (combine tmpDir "data/Template.hs"))
            res <- fit tmpDir $
              file "Biegunka.hs"
                & contents ?~ "bar"
                & mode ?~ 0o100644
            res `shouldHave` _Right

{-# LANGUAGE OverloadedStrings #-}
module RunSpec (spec) where

import Control.Lens
import Data.List.Lens
import System.Directory.Layout
import Test.Hspec.Lens

import Run (find)
import SpecHelper (withBiegunkaTempDirectory)


spec :: Spec
spec =
  describe "find" $
    around withBiegunkaTempDirectory $ do
      it "deeply traverses the directory looking for files name ‘Biegunka.hs’" $ \tmpDir -> do
        make tmpDir $ do
          dir "foo" $
            file "Biegunka.hs"
              & contents ?~ ""
          dir "bar" $ do
            dir "baz" $
              file "Biegunka.hs"
                & contents ?~ ""
            dir "qux" $
              emptydir "quux"
          file "Biegunka.hs"
            & contents ?~ ""
        res <- find tmpDir
        res `shouldList` ["Biegunka.hs", "bar/baz/Biegunka.hs", "foo/Biegunka.hs"]
            `through` traverse.prefixed tmpDir.prefixed "/"

      it "ignores the directories whose name starts with a dot" $ \tmpDir -> do
        make tmpDir $ do
          dir "foo" $
            file "Biegunka.hs"
              & contents ?~ ""
          dir "bar" $ do
            dir ".baz" $
              file "Biegunka.hs"
                & contents ?~ ""
            dir "qux" $
              emptydir "quux"
          file "Biegunka.hs"
            & contents ?~ ""
        res <- find tmpDir
        res `shouldList` ["Biegunka.hs", "foo/Biegunka.hs"]
            `through` traverse.prefixed tmpDir.prefixed "/"

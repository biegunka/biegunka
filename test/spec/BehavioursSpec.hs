{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module BehavioursSpec (spec) where

import           Control.Biegunka
import           Control.Biegunka.Source.Layout (layout)
import qualified Control.Biegunka.Source.Directory as D
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty)
import           System.Directory.Layout
  ( Layout, FitError
  , make, fit
  , fromErrors
  , dir, emptydir, file, symlink, contents, exists
  )
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Hspec

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


spec :: Spec
spec = do
  describe "Trivial biegunka script" $
    it "should be trivial layout too" $
      (trivial_script `fitsWith` trivial_layout) `shouldReturn` fromErrors []

  describe "Trivial biegunka namespace script" $
    it "should be trivial layout too" $
      (trivial_repo "biegunka-test" `fitsWith` trivial_layout) `shouldReturn` fromErrors []

  describe "Simple biegunka namespace script" $ do
    it "should be simple layout too" $
      (simple_repo_0 `fitsWith` simple_layout_0) `shouldReturn` fromErrors []

    it "should disappear after deletion" $
      (trivial_repo "biegunka-simple0" `fitsWith` trivial_layout) `shouldReturn` fromErrors []

  describe "Simple biegunka no namespace script" $ do
    it "should be simple layout too" $
      (simple_repo_no_namespace_0 `fitsWith` simple_layout_no_namespace_0) `shouldReturn` fromErrors []

    it "should disappear after deletion" $
      (trivial_repo "" `fitsWith` trivial_layout) `shouldReturn` fromErrors []

  describe "Simple copying" $ do
    it "should copy the directory correctly" $
      withBiegunkaDirectory $ \tmp -> do
        make tmp (dir "a" simple_copying_layout_0)
        biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run $
          D.directory tmp $ do
            copy "a/foo"      (tmp </> "b/foo")
            copy "a/bar"      (tmp </> "b/bar")
            copy "a/baz/quux" (tmp </> "b/baz/quux")
        fit tmp (dir "b" simple_copying_layout_0)
     `shouldReturn`
      fromErrors []

    it "should disappear after deletion" $
      (trivial_repo "" `fitsWith` trivial_layout) `shouldReturn` fromErrors []

  describe "Simple registering" $
    it "should link the repository correctly" $
      (simple_repo_registering `fitsWith` simple_layout_registering) `shouldReturn` fromErrors []

fitsWith
  :: (FilePath -> Script 'Sources ())
  -> (FilePath -> Layout a)
  -> IO (Either (NonEmpty FitError) ())
fitsWith s l =
  withBiegunkaDirectory $ \tmp -> do
    biegunka (set runRoot tmp . set biegunkaRoot (tmp </> ".biegunka")) run (s tmp)
    fit tmp (l tmp)

trivial_script :: FilePath -> Script 'Sources ()
trivial_script _ = return ()

trivial_layout :: FilePath -> Layout ()
trivial_layout _ = return ()

trivial_repo :: String -> FilePath -> Script 'Sources ()
trivial_repo tmp _ = namespace tmp $ return ()

simple_repo_0 :: FilePath -> Script 'Sources ()
simple_repo_0 tmp =
  namespace "biegunka-simple0" $
    layout l (tmp </> "biegunka-simple0") $
      copy "src0" (tmp </> "dst0")
 where
  l = file "src0" & contents ?~ "thisiscontents\n"

simple_layout_0 :: FilePath -> Layout ()
simple_layout_0 tmp =
  dir tmp $
    file "dst0"
      & contents ?~ "thisiscontents\n"

simple_repo_no_namespace_0 :: FilePath -> Script 'Sources ()
simple_repo_no_namespace_0 tmp =
  layout l (tmp </> "biegunka-simple0") $
    copy "src0" (tmp </> "dst0")
 where
  l = file "src0" & contents ?~ "thisiscontents\n"

simple_layout_no_namespace_0 :: FilePath -> Layout ()
simple_layout_no_namespace_0 tmp =
  dir tmp $
    file "dst0"
      & contents ?~ "thisiscontents\n"

simple_copying_layout_0 :: Layout ()
simple_copying_layout_0 = do
  file "foo"
    & contents ?~ "foocontents\n"
  file "bar"
    & contents ?~ "barcontents\n"
  dir "baz" $
    file "quux"
      & contents ?~ "quuxcontents\n"

simple_repo_registering :: FilePath -> Script 'Sources ()
simple_repo_registering tmp =
  layout (return ()) (tmp </> "foo") $
    register "bar"

simple_layout_registering :: FilePath -> Layout ()
simple_layout_registering tmp = do
  emptydir "foo"
  symlink "bar" (tmp </> "foo")
    & exists .~ True


withBiegunkaDirectory :: (FilePath -> IO a) -> IO a
withBiegunkaDirectory = withSystemTempDirectory "biegunka-"

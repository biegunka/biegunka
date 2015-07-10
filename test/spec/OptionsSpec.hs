{-# LANGUAGE LambdaCase #-}
module OptionsSpec (spec) where

import Control.Lens
import System.Exit.Lens
import Test.Hspec.Lens

import Options


spec :: Spec
spec =
  describe "parser" $ do
    it "has ’version’ subcommand" $
      parseArgs ["version"] `shouldHave` _Version

    it "has ’help’ subcommand" $
      parseArgs ["help"] `shouldHave` _Help._3._ExitSuccess

    context "‘init’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["init", "foo"] `shouldHave` _Init.only "foo"

      it "filename argument is optional" $
        parseArgs ["init"] `shouldHave` _Init.only "."

    context "‘run’ subcommand" $ do
      it "takes a filename argument" $ do
        parseArgs ["run", "Foo.hs"] `shouldHave` _Run.only (Just "Foo.hs", [])
        parseArgs ["run", "Foo.hs", "--foo"] `shouldHave` _Run.only (Just "Foo.hs", ["--foo"])
        parseArgs ["run", "Foo.hs", "--"] `shouldHave` _Run.only (Just "Foo.hs", [])
        parseArgs ["run", "Foo.hs", "--", "foo", "--bar"] `shouldHave` _Run.only (Just "Foo.hs", ["foo", "--bar"])

      it "filename argument is optional" $ do
        parseArgs ["run"] `shouldHave` _Run.only (Nothing, [])
        parseArgs ["run", "--foo"] `shouldHave` _Run.only (Nothing, ["--foo"])
        parseArgs ["run", "--"] `shouldHave` _Run.only (Nothing, [])
        parseArgs ["run", "--", "foo", "--bar"] `shouldHave` _Run.only (Nothing, ["foo", "--bar"])

    context "‘json’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["json", "foo"] `shouldHave` _Json.only "foo"

      it "filename argument is optional" $
        parseArgs ["json"] `shouldHave` _Json.only defaultBiegunkaDataDirectory

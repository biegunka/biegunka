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
        parseArgs ["init", "."] `shouldHave` _Init.only "."

      it "filename argument is mandatory" $
        parseArgs ["init"] `shouldHave` _Help._3._ExitFailure

    context "‘run’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["run", "Foo.hs", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Run.only (Just "Foo.hs", ["foo", "--bar", "baz"])

      it "filename argument is optional" $
        parseArgs ["run", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Run.only (Nothing, ["foo", "--bar", "baz"])

    context "‘json’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["json", "foo"] `shouldHave` _Json.only "foo"

      it "filename argument is optional" $
        parseArgs ["json"] `shouldHave` _Json.only defaultBiegunkaDataDirectory

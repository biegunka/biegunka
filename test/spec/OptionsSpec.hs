{-# LANGUAGE LambdaCase #-}
module OptionsSpec (spec) where

import Control.Lens
import Test.Hspec.Lens

import Options


spec :: Spec
spec =
  describe "parser" $ do
    it "has ’version’ subcommand" $
      parseArgs ["version"] `shouldHave` _Right._Version

    it "has ’help’ subcommand" $
      parseArgs ["help"] `shouldHave` _Right._Help

    context "‘init’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["init", "."] `shouldHave` _Right._Init.only "."

      it "filename argument is mandatory" $
        parseArgs ["init"] `shouldHave` _Left

    context "‘run’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["run", "Foo.hs", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Right._Run.only (Just "Foo.hs", ["foo", "--bar", "baz"])

      it "filename argument is optional" $
        parseArgs ["run", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Right._Run.only (Nothing, ["foo", "--bar", "baz"])

    context "‘json’ subcommand" $ do
      it "takes a filename argument" $
        parseArgs ["json", "foo"] `shouldHave` _Right._Json.only "foo"

      it "filename argument is optional" $
        parseArgs ["json"] `shouldHave` _Right._Json.only defaultBiegunkaDataDirectory

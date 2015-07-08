{-# LANGUAGE LambdaCase #-}
module OptionsSpec (spec) where

import Control.Lens
import Test.Hspec.Lens

import Options


spec :: Spec
spec =
  describe "parser" $ do
    it "has ’version’ subcommand" $
      parse ["version"] `shouldHave` _Right._Version

    it "has ’help’ subcommand" $
      parse ["help"] `shouldHave` _Right._Help

    context "‘init’ subcommand" $ do
      it "takes a filename argument" $
        parse ["init", "."] `shouldHave` _Right._Init.only "."

      it "filename argument is mandatory" $
        parse ["init"] `shouldHave` _Left

    context "‘run’ subcommand" $ do
      it "takes a filename argument" $
        parse ["run", "Foo.hs", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Right._Run.only (Just "Foo.hs", ["foo", "--bar", "baz"])

      it "filename argument is optional" $
        parse ["run", "--", "foo", "--bar", "baz"]
       `shouldHave`
        _Right._Run.only (Nothing, ["foo", "--bar", "baz"])

    context "‘json’ subcommand" $ do
      it "takes a filename argument" $
        parse ["json", "foo"] `shouldHave` _Right._Json.only "foo"

      it "filename argument is optional" $
        parse ["json"] `shouldHave` _Right._Json.only defaultBiegunkaDataDirectory

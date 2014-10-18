{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Biegunka.OptionsSpec (spec) where

import Control.Lens
import Data.Data (Typeable, Data)
import Data.Foldable (for_)
import Data.Monoid (mempty)
import Options.Applicative
import Test.Hspec.Lens
import Text.Printf (printf)

import Control.Biegunka.Options


data Type = Foo Int | FooBarBaz deriving (Show, Eq, Typeable, Data)

spec :: Spec
spec = do
  describe "transformConstructor" $ do
    it "does not change lowercase constructors" $
      transformConstructor "foobar" `shouldBe` "foobar"

    it "replaces uppercase letter with lowercase letter prefixed by a hyphen" $
      transformConstructor "fooBar" `shouldBe` "foo-bar"

    it "replaces first uppercase letter with lowercase letter" $
      transformConstructor "FooBar" `shouldBe` "foo-bar"

  describe "constructorName" $ do
    it "returns transformed simple constructor name" $
      constructorName (Foo 4) `shouldBe` "foo"

    it "returns transformed complex constructor name" $
      constructorName FooBarBaz `shouldBe` "foo-bar-baz"

  describe "constructorOption" $ do
    let constructorOption' = snd . constructorOption mempty

    it "returns a parser that parses simple constructor option" $
      parse (constructorOption' (Foo 4)) ["--foo"] `shouldHave` _Just.only (Foo 4)

    it "returns a parser that parses complex constructor option" $
      parse (constructorOption' FooBarBaz) ["--foo-bar-baz"] `shouldHave` _Just.only FooBarBaz

  describe "constructorOptions" $ do
    let f = constructorOptions [Foo 1, Foo 2, Foo 3, FooBarBaz]

    it "returns a parser that parses simple constructor option" $
      parse f ["--foo"] `shouldHave` _Just.only (Foo 1)

    it "returns a parser that parses complex constructor option" $
      parse f ["--foo-bar-baz"] `shouldHave` _Just.only FooBarBaz

    it "adds a suffix for a second encounter of the same constructor" $
      parse f ["--foo-1"] `shouldHave` _Just.only (Foo 2)

    it "adds a suffix for a third encounter of the same constructor" $
      parse f ["--foo-2"] `shouldHave` _Just.only (Foo 3)

  describe "parser" $ do
    let f = parser [Foo 4, FooBarBaz]

    it "returns a parser that parses simple constructor option" $
      parse f ["--foo"] `shouldHave` _Just._1.only (Foo 4)

    it "returns a parser that parses complex constructor option" $
      parse f ["--foo-bar-baz"] `shouldHave` _Just._1.only FooBarBaz

    context "interpreters" $
      for_ ["--changes", "--run", "--problems", "--force", "--all"] $ \o ->
        it (printf "returns a parser that has %s option" o) $
          parse f [o, "--foo"] `shouldHave` _Just

    context "modes" $
      for_ ["--online", "--offline"] $ \o ->
        it (printf "returns a parser that has %s option" o) $
          parse f [o, "--foo"] `shouldHave` _Just

parse :: Parser a -> [String] -> Maybe a
parse p = getParseResult . execParserPure (prefs mempty) (info p fullDesc)

instance Show (a -> b) where
  show _ = "<Function>"

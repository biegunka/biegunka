{-# LANGUAGE DeriveGeneric #-}
module Control.Biegunka.OptionsSpec (spec) where

import Control.Lens
import Data.Foldable (for_)
import GHC.Generics
import Options.Applicative
import Test.Hspec.Lens
import Text.Show.Functions ()

import Control.Biegunka.Options


data Type = Foo | QuxQuux
    deriving (Show, Eq, Generic)

instance Environments Type

spec :: Spec
spec =
  describe "parser" $ do
    context "when asked for a single-word environment" $
      it "returns the corresponding constructor" $
        parse ["--foo"] `shouldHave` _Just._1.only Foo

    context "when asked for a multi-word environment" $
      it "returns the corresponding constructor" $
        parse ["--qux-quux"] `shouldHave` _Just._1.only QuxQuux

    describe "interpreters" $
      for_ ["--diff", "--run", "--problems", "--force", "--all"] $ \i ->
        it ("parser includes the ‘" ++ i ++ "’ interpreter") $
          parse [i, "--foo"] `shouldHave` _Just._1.only Foo

    describe "modes" $
      for_ ["--online", "--offline"] $ \m ->
        it ("parser includes the ‘" ++ m ++ "’ mode") $
          parse [m, "--foo"] `shouldHave` _Just._1.only Foo

parse :: Environments a => [String] -> Maybe (a, Runner b)
parse = getParseResult . execParserPure (prefs idm) (parser environments)

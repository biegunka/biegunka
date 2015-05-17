{-# LANGUAGE DeriveGeneric #-}
module Control.Biegunka.OptionsSpec (spec) where

import Control.Lens
import Data.Proxy (Proxy(Proxy))
import Data.Foldable (for_)
import GHC.Generics
import Options.Applicative
import Test.Hspec.Lens
import Text.Show.Functions ()

import Control.Biegunka.Options


data Type = Foo | QuxQuux
    deriving (Show, Eq, Bounded, Enum, Generic)

instance Environments Type

spec :: Spec
spec = do
  describe "parser" $ do
    context "when asked for a single-word environment" $
      it "returns the corresponding constructor" $
        parse Proxy ["--foo"] `shouldHave` _Just._1.only Foo

    context "when asked for a multi-word environment" $
      it "returns the corresponding constructor" $
        parse Proxy ["--qux-quux"] `shouldHave` _Just._1.only QuxQuux

    describe "interpreters" $
      for_ ["--changes", "--run", "--problems", "--force", "--all"] $ \i ->
        it ("parser includes the ‘" ++ i ++ "’ interpreter") $
          parse Proxy [i, "--foo"] `shouldHave` _Just._1.only Foo

    describe "modes" $
      for_ ["--online", "--offline"] $ \m ->
        it ("parser includes the ‘" ++ m ++ "’ mode") $
          parse Proxy [m, "--foo"] `shouldHave` _Just._1.only Foo

parse :: Environments a => proxy a -> [String] -> Maybe (a, Runner b)
parse proxy = getParseResult . execParserPure (prefs idm) (parser (fromEnvironments proxy))

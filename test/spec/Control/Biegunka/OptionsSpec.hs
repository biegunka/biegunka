{-# LANGUAGE DeriveGeneric #-}
module Control.Biegunka.OptionsSpec (spec) where

import Data.Foldable (for_)
import GHC.Generics
import Options.Applicative
import Test.Hspec.Lens
import Text.Printf (printf)

import Control.Biegunka.Options


data Foo = Bar | QuxQuux | Xyzzy Int
    deriving (Show, Eq, Generic)

instance Environments Foo

spec :: Spec
spec =
  describe "parser" $ do
    it "single-word environments are selected by lowercase flags" $
      parse ["--bar"] `shouldBe` Just Bar

    it "multi-word environments are selected by lowercase lisp-case flags" $
      parse ["--qux-quux"] `shouldBe` Just QuxQuux

    it "environments taking an argument are selected by lowercase options" $
      parse ["--xyzzy", "7"] `shouldBe` Just (Xyzzy 7)

    describe "interpreters" $
      for_ ["--diff", "--run", "--problems", "--force", "--all"] $ \i ->
        it (printf "parser includes the ‘%s’ interpreter" i) $
          parse [i, "--bar"] `shouldBe` Just Bar

    describe "modes" $
      for_ ["--online", "--offline"] $ \m ->
        it (printf "parser includes the ‘%s’ mode" m) $
          parse [m, "--bar"] `shouldBe` Just Bar

parse :: Environments a => [String] -> Maybe a
parse = fmap fst . getParseResult . execParserPure (prefs idm) (parser environments)

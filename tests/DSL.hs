{-# LANGUAGE GADTs #-}
module Main where

import Data.Monoid (mempty)

import Control.Lens
import Control.Monad.Free (Free(..))
import Data.Default (def)
import Data.Foldable (toList)
import Biegunka.Language (Term(..), Action(..), Source(..), Modifier(..))
import Biegunka.Primitive
import Biegunka.Script (Annotate(..), evalScript, app, source)
import Biegunka.Source.Layout (layout_)
import Biegunka.Source.Directory (directory)
import Test.Hspec


main :: IO ()
main = hspec $
  describe "Biegunka DSL" $ do
    context "chaining" $ do
      it "gives unchained tasks different ids" $
        let ast = evalScript def (layout_ mempty mempty >> layout_ mempty mempty)
        in case ast of
          Free (TS (AS { asToken = s })  _ (Pure ())
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))) -> s /= t
          _ -> False
      it "gives chained tasks different ids" $
        let ast = evalScript def (layout_ mempty mempty `prerequisiteOf` layout_ mempty mempty)
        in case ast of
          Free (TS (AS { asToken = s })  _ (Pure ())
            (Free (TM _
              (Free (TS (AS { asToken = t }) _ (Pure ())
                (Pure ())))))) -> s /= t
          _ -> False
      it "gives Wait modifier correct tasks ids" $
        let ast = evalScript def (layout_ mempty mempty <~> layout_ mempty mempty)
        in case ast of
          Free (TS _ _ (Pure ())
            (Free (TM (Wait ids)
              (Free (TS (AS { asToken = t }) _ (Pure ())
                (Pure ())))))) -> toList ids `shouldBe` [0 .. t - 1]
          _ -> expectationFailure "DSL pattern failed"
    context "relative paths" $ do
      it "mangles relative paths for Actions" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (link "from" "to")
        in case ast of
          Free (TA _ (Link "source/from" "app/to") (Pure ())) -> True
          _ -> False
      it "mangles relative paths for Sources" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (layout_ mempty "to")
        in case ast of
          Free (TS _ (Source { spath = "app/to" }) (Pure ()) (Pure ())) -> True
          _ -> False
    context "absolute paths" $ do
      it "does not mangle absolute paths for Actions" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (link "from" "/to")
        in case ast of
          Free (TA _ (Link "source/from" "/to") (Pure ())) -> True
          _ -> False
      it "does not mangle absolute paths for Sources" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (layout_ mempty "/to")
        in case ast of
          Free (TS _ (Source { spath = "/to" }) (Pure ()) (Pure ())) -> True
          _ -> False
    context "profiles" $ do
      it "does not matter how nested profiles are constructed" $
        let ast = evalScript def $
              profile "foo" $
                group "bar" $
                  group "baz" $
                    directory "/" (return ())
            ast' = evalScript def $
              profile "foo/bar/baz" $
                directory "/" (return ())
        in case (ast, ast') of
          ( Free (TS AS { asProfile = p  } Source {} (Pure ()) (Pure ()))
           , Free (TS AS { asProfile = p' } Source {} (Pure ()) (Pure ()))
           ) -> p == p'
          _ -> False

    it "does something useful" $ pending

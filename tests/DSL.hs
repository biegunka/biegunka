{-# LANGUAGE GADTs #-}
module Main where

import Data.Monoid (mempty)

import Control.Lens
import Control.Monad.Free (Free(..))
import Data.Default (def)
import Biegunka.Language (Term(..), Action(..), Source(..))
import Biegunka.Primitive (chain, (<~>), link)
import Biegunka.Script (Annotate(..), evalScript, app, source)
import Biegunka.Source.Layout (layout_)
import Test.Hspec (hspec, describe, context, it, pending)


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
      it "gives chained tasks the same id" $
        let ast = evalScript def (layout_ mempty mempty `chain` layout_ mempty mempty)
        in case ast of
          Free (TS (AS { asToken = s })  _ (Pure ())
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))) -> s == t
          _ -> False
      it "gives chained tasks the same id (infix)" $
        let ast = evalScript def (layout_ mempty mempty <~> layout_ mempty mempty)
        in case ast of
          Free (TS (AS { asToken = s })  _ (Pure ())
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))) -> s == t
          _ -> False
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

    it "does something useful" $ pending

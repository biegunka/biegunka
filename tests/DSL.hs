{-# LANGUAGE GADTs #-}
module Main where

import Data.Monoid (mempty)

import Control.Lens
import Control.Monad.Free (Free(..))
import Data.Default (def)
import Biegunka.Language (EL(..), A(..), S(..))
import Biegunka.Primitive (chain, link)
import Biegunka.Script (SA(..), evalScript, app, source)
import Biegunka.Source.Dummy (dummy_)
import Test.Hspec (hspec, describe, context, it, pending)


main :: IO ()
main = hspec $
  describe "Biegunka DSL" $ do
    context "chaining" $ do
      it "gives unchained tasks different ids" $
        let ast = evalScript def (dummy_ mempty mempty >> dummy_ mempty mempty)
        in case ast of
          Free (ES (SAS { sasToken = s })  _ (Pure ())
            (Free (ES (SAS { sasToken = t }) _ (Pure ())
              (Pure ())))) -> s /= t
          _ -> False
      it "gives chained tasks the same id" $
        let ast = evalScript def (dummy_ mempty mempty `chain` dummy_ mempty mempty)
        in case ast of
          Free (ES (SAS { sasToken = s })  _ (Pure ())
            (Free (ES (SAS { sasToken = t }) _ (Pure ())
              (Pure ())))) -> s == t
          _ -> False
    context "paths" $ do
      it "mangles paths for Actions" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (link "from" "to")
        in case ast of
          Free (EA _ (Link "source/from" "app/to") (Pure ())) -> True
          _ -> False
      it "mangles paths for Sources" $
        let ast = evalScript (def & app .~ "app" & source .~ "source") (dummy_ mempty "to")
        in case ast of
          Free (ES _ (S { spath = "app/to" }) (Pure ()) (Pure ())) -> True
          _ -> False

    it "does something useful" $ pending

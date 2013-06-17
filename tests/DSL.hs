{-# LANGUAGE GADTs #-}
module Main where

import Data.Monoid (mempty)

import Control.Monad.Free (Free(..))
import Data.Default (def)
import Biegunka.Language (EL(..))
import Biegunka.Primitive (chain)
import Biegunka.Script (SA(..), evalScript)
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
    it "does something useful" $ pending

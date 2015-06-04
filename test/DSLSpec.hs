{-# LANGUAGE GADTs #-}
module DSLSpec (spec) where

import Control.Lens
import Control.Monad.Free (Free(..))
import Data.Foldable (toList)
import Test.Hspec

import Control.Biegunka.Language (Term(..), Action(..), Source(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script
import Control.Biegunka.Source.Directory (directory)


spec :: Spec
spec = describe "Biegunka DSL" $ do
  context "chaining" $ do
    it "gives unchained tasks different ids" $
      let ast = evalScript defaultMAnnotations defaultAnnotations (directory "/" (return ()) >> directory "/" (return ()))
      in case ast of
        Free (TS (AS { asToken = s })  _ (Pure ())
          (Free (TS (AS { asToken = t }) _ (Pure ())
            (Pure ())))) -> s /= t
        _ -> False
    it "gives chained tasks different ids" $
      let ast = evalScript defaultMAnnotations defaultAnnotations (directory "/" (return ()) `prerequisiteOf` directory "/" (return ()))
      in case ast of
        Free (TS (AS { asToken = s })  _ (Pure ())
          (Free (TWait _
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))))) -> s /= t
        _ -> False
    it "gives Wait modifier correct tasks ids" $
      let ast = evalScript defaultMAnnotations defaultAnnotations (directory "/" (return ()) <~> directory "/" (return ()))
      in case ast of
        Free (TS _ _ (Pure ())
          (Free (TWait ids
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))))) -> toList ids `shouldBe` [toEnum 0 .. pred t]
        _ -> expectationFailure "DSL pattern failed"
  context "relative paths" $ do
    it "mangles relative paths for Actions" $
      let ast = evalScript defaultMAnnotations (defaultAnnotations & set runRoot "app" & set sourceRoot "source") (link "from" "to")
      in case ast of
        Free (TA _ (Link "source/from" "app/to") (Pure ())) -> True
        _ -> False
    it "mangles relative paths for Sources" $
      let ast = evalScript defaultMAnnotations (defaultAnnotations & set runRoot "app" & set sourceRoot "source") (directory "to" (return ()))
      in case ast of
        Free (TS _ (Source { spath = "app/to" }) (Pure ()) (Pure ())) -> True
        _ -> False
  context "absolute paths" $ do
    it "does not mangle absolute paths for Actions" $
      let ast = evalScript defaultMAnnotations (defaultAnnotations & set runRoot "app" & set sourceRoot "source") (link "from" "/to")
      in case ast of
        Free (TA _ (Link "source/from" "/to") (Pure ())) -> True
        _ -> False
    it "does not mangle absolute paths for Sources" $
      let ast = evalScript defaultMAnnotations (defaultAnnotations & set runRoot "app" & set sourceRoot "source") (directory "/to" (return ()))
      in case ast of
        Free (TS _ (Source { spath = "/to" }) (Pure ()) (Pure ())) -> True
        _ -> False
  context "namespaces" $
    it "does not matter how nested namespaces are constructed" $
      let ast = evalScript defaultMAnnotations defaultAnnotations $
            namespace "foo" $
              namespace "bar" $
                namespace "baz" $
                  directory "/" (return ())
          ast' = evalScript defaultMAnnotations defaultAnnotations $
            namespace "foo/bar/baz" $
              directory "/" (return ())
      in case (ast, ast') of
        ( Free (TS AS { asSegments = _  } Source {} (Pure ()) (Pure ()))
         , Free (TS AS { asSegments = _ } Source {} (Pure ()) (Pure ()))
         ) -> pendingWith "this example should probably be removed"
        _ -> expectationFailure "bad"

  it "does something useful" pending

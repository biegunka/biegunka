{-# LANGUAGE GADTs #-}
module DSLSpec (spec) where

import Data.Foldable (toList)

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Data.Default.Class (def)
import qualified Data.Set as S
import           Test.Hspec

import Control.Biegunka.Language (Term(..), Action(..), Source(..), Modifier(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script
import Control.Biegunka.Script.Token (tokens, noTokens)
import Control.Biegunka.Source.Directory (directory)


spec :: Spec
spec = describe "Biegunka DSL" $ do
  context "chaining" $ do
    it "gives unchained tasks different ids" $
      let ast = evalScript def def tokens (directory "/" (return ()) >> directory "/" (return ()))
      in case ast of
        Free (TS (AS { asToken = s })  _ (Pure ())
          (Free (TS (AS { asToken = t }) _ (Pure ())
            (Pure ())))) -> s /= t
        _ -> False
    it "gives chained tasks different ids" $
      let ast = evalScript def def tokens (directory "/" (return ()) `prerequisiteOf` directory "/" (return ()))
      in case ast of
        Free (TS (AS { asToken = s })  _ (Pure ())
          (Free (TM _
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))))) -> s /= t
        _ -> False
    it "gives Wait modifier correct tasks ids" $
      let ast = evalScript def def tokens (directory "/" (return ()) <~> directory "/" (return ()))
      in case ast of
        Free (TS _ _ (Pure ())
          (Free (TM (Wait ids)
            (Free (TS (AS { asToken = t }) _ (Pure ())
              (Pure ())))))) -> toList ids `shouldBe` [toEnum 0 .. pred t]
        _ -> expectationFailure "DSL pattern failed"
  context "relative paths" $ do
    it "mangles relative paths for Actions" $
      let ast = evalScript def (def & app .~ "app" & sourcePath .~ "source") noTokens (link "from" "to")
      in case ast of
        Free (TA _ (Link "source/from" "app/to") (Pure ())) -> True
        _ -> False
    it "mangles relative paths for Sources" $
      let ast = evalScript def (def & app .~ "app" & sourcePath .~ "source") tokens (directory "to" (return ()))
      in case ast of
        Free (TS _ (Source { spath = "app/to" }) (Pure ()) (Pure ())) -> True
        _ -> False
  context "absolute paths" $ do
    it "does not mangle absolute paths for Actions" $
      let ast = evalScript def (def & app .~ "app" & sourcePath .~ "source") noTokens (link "from" "/to")
      in case ast of
        Free (TA _ (Link "source/from" "/to") (Pure ())) -> True
        _ -> False
    it "does not mangle absolute paths for Sources" $
      let ast = evalScript def (def & app .~ "app" & sourcePath .~ "source") tokens (directory "/to" (return ()))
      in case ast of
        Free (TS _ (Source { spath = "/to" }) (Pure ()) (Pure ())) -> True
        _ -> False
  context "profiles" $ do
    it "does not matter how nested profiles are constructed" $
      let ast = evalScript def def tokens $
            profile "foo" $
              group "bar" $
                group "baz" $
                  directory "/" (return ())
          ast' = evalScript def def tokens $
            profile "foo/bar/baz" $
              directory "/" (return ())
      in case (ast, ast') of
        ( Free (TS AS { asProfile = p  } Source {} (Pure ()) (Pure ()))
         , Free (TS AS { asProfile = p' } Source {} (Pure ()) (Pure ()))
         ) -> p == p'
        _ -> False
    it "collects all mentioned profiles no matter what" $
      let (_, as) = runScript def def tokens $ do
            profile "foo" $ do
              group "bar" $
                directory "/" (return ())
              directory "/" (return ())
            profile "baz" $
              directory "/" (return ())
            profile "quux" $
              return ()
            directory "/" (return ())
      in view profiles as == S.fromList ["foo", "foo/bar", "baz", "quux", ""]
    it "ignores \"\" if no ungrouped source is mentioned" $
      let (_, as) = runScript def def tokens $ do
            profile "baz" $
              directory "/" (return ())
            profile "quux" $
              return ()
      in view profiles as == S.fromList ["baz", "quux"]

  it "does something useful" pending

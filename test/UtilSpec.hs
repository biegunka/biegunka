module UtilSpec
  ( spec
  ) where

import Test.Hspec

import Control.Biegunka.Biegunka (expandHome)


spec :: Spec
spec =
  describe "expandHome" $ do

    it "expands ~ to user home directory in \"~\" pattern" $ do

      home <- expandHome "~"
      home `shouldSatisfy` (\h -> h /= "~")

    it "expands ~ to user home directory in \"~/foo\" pattern" $ do

      home <- expandHome "~/foo"
      home `shouldSatisfy` (\h -> h /= "~/foo")

    it "does is 'id' for other patterns" $ do

      expandHome "/foo/bar" `shouldReturn` "/foo/bar"
      expandHome "baz/quux" `shouldReturn` "baz/quux"

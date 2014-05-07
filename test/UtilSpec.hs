module UtilSpec
  ( spec
  ) where

import           System.FilePath ((</>))
import qualified System.Posix as Posix
import           Test.Hspec

import           Control.Biegunka.Biegunka (expandHome)


spec :: Spec
spec =
  describe "expandHome" $ do
    it "expands bare ~ to the user home directory" $ do
      dir <- expandHome "~"
      home <- getHome
      dir `shouldBe` home

    it "expands ~/$path to $path in the user home directory" $ do
      dir <- expandHome "~/foo"
      home <- getHome
      dir `shouldBe` home </> "foo"

    it "expands ~$user to the $user user home directory" $ do
      name <- getName
      dir <- expandHome ("~" ++ name)
      home <- getHome
      dir `shouldBe` home

    it "expands ~$user/$path to $path in the $user user home directory" $ do
      name <- getName
      dir <- expandHome ("~" ++ name ++ "/foo")
      home <- getHome
      dir `shouldBe` home </> "foo"

    it "is 'id' for other absolute patterns" $
      expandHome "/foo/bar~" `shouldReturn` "/foo/bar~"

    it "is 'id' for other relative patterns" $
      expandHome "baz/qu~ux" `shouldReturn` "baz/qu~ux"

getHome :: IO FilePath
getHome = fmap Posix.homeDirectory . Posix.getUserEntryForID =<< Posix.getEffectiveUserID

getName :: IO String
getName = fmap Posix.userName . Posix.getUserEntryForID =<< Posix.getEffectiveUserID

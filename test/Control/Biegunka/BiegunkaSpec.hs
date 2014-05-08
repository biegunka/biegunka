module Control.Biegunka.BiegunkaSpec
  ( spec
  ) where

import           Control.Exception (bracket)
import           Data.Maybe (fromMaybe)
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import qualified System.Posix as Posix
import           Test.Hspec

import           Control.Biegunka.Biegunka (expandHome)


spec :: Spec
spec =
  describe "expandHome" $ do
    it "expands bare ~ to the user home directory" $ do
      dir <- expandHome "~"
      home <- getHomeDirectory
      dir `shouldBe` home

    it "reflects changes to HOME" $
      bracket (Posix.getEnv "HOME") (\home -> Posix.putEnv ("HOME=" ++ fromMaybe "" home)) $ \_ -> do
        Posix.putEnv "HOME=foo"
        dir <- expandHome "~"
        dir `shouldBe` "foo"

    it "expands ~/$path to $path in the user home directory" $ do
      dir <- expandHome "~/foo"
      home <- getHomeDirectory
      dir `shouldBe` home </> "foo"

    it "expands ~$user to the $user user home directory" $ do
      name <- getName
      dir <- expandHome ("~" ++ name)
      home <- getHomeDirectory
      dir `shouldBe` home

    it "expands ~$user/$path to $path in the $user user home directory" $ do
      name <- getName
      dir <- expandHome ("~" ++ name ++ "/foo")
      home <- getHomeDirectory
      dir `shouldBe` home </> "foo"

    it "is 'id' for other absolute patterns" $
      expandHome "/foo/bar~" `shouldReturn` "/foo/bar~"

    it "is 'id' for other relative patterns" $
      expandHome "baz/qu~ux" `shouldReturn` "baz/qu~ux"

getName :: IO String
getName = fmap Posix.userName . Posix.getUserEntryForID =<< Posix.getEffectiveUserID

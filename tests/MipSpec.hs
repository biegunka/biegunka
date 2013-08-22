module MipSpec (spec) where

import Control.Lens
import Prelude hiding (lookup)
import Test.Hspec

import Control.Biegunka.Execute.Settings


spec :: Spec
spec = do

  describe "Mip construction" $ do

    it "can be constructed 'manually'" $ do
      let empty, nonempty :: Mip Int Int
          empty    = Empty
          nonempty = Mip 4 7

      -- singleton
      singleton 4 7 `shouldBe` nonempty

      -- lists
      fromList []               `shouldBe` empty
      fromList [(4, 7)]         `shouldBe` nonempty
      fromList [(4, 7), (3, 2)] `shouldBe` nonempty
      fromList [(4, 5), (4, 7)] `shouldBe` nonempty

  describe "Mip queries" $ do
    let mip :: Mip String Int
        mip = Mip "hello" 7

    it "can be queried 'manually'" $ do

      lookup "hello" mip `shouldBe` Just 7
      lookup "world" mip `shouldBe` Nothing

    it "can be queried with lens" $ do

      mip ^. at "hello" `shouldBe` Just 7
      mip ^. at "world" `shouldBe` Nothing
      mip ^? ix "hello" `shouldBe` Just 7
      mip ^? ix "world" `shouldBe` Nothing

  describe "Mip changes" $ do
    let empty, nonempty :: Mip String Int
        empty    = Empty
        nonempty = Mip "hello" 7

    it "can be changed 'manually'" $ do

      insert "hello" 7 empty     `shouldBe` nonempty
      insert "hello" 11 nonempty `shouldBe` Mip "hello" 11
      insert "world" 11 nonempty `shouldBe` nonempty

      delete "hello" empty    `shouldBe` empty
      delete "hello" nonempty `shouldBe` empty
      delete "world" nonempty `shouldBe` nonempty

    it "can be changed with lens" $ do

      (empty & at "hello" ?~ 7)     `shouldBe` nonempty
      (nonempty & at "hello" ?~ 11) `shouldBe` Mip "hello" 11
      (nonempty & at "world" ?~ 11) `shouldBe` nonempty

      (empty & at "hello" .~ Nothing)    `shouldBe` empty
      (nonempty & at "hello" .~ Nothing) `shouldBe` empty
      (nonempty & at "world" .~ Nothing) `shouldBe` nonempty

  describe "Mip stats" $ do
    let empty, nonempty :: Mip String Int
        empty    = Empty
        nonempty = Mip "hello" 7

    it "is possible to get some information about Mip" $ do

      elems empty    `shouldBe` Nothing
      elems nonempty `shouldBe` Just 7

      keys empty    `shouldBe` Nothing
      keys nonempty `shouldBe` Just "hello"

      assocs empty    `shouldBe` Nothing
      assocs nonempty `shouldBe` Just ("hello", 7)

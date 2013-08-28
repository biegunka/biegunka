{-# LANGUAGE OverloadedStrings #-}
module GenerateSpec where

import           Data.Char (isSpace)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import           Test.Hspec

import Control.Biegunka.Groups

import Generate


spec :: Spec
spec = do

  describe "imports generation" $ do

    it "extracts all unique source types" $ do

      let defSR = SR { sourceType = "", sourcePath = "", fromLocation = "", sourceOwner = Nothing }
          db = M.fromList
            [ ("foo", GR $ M.fromList
              [ (defSR { sourceType = "git",   sourcePath = "a" }, S.empty)
              , (defSR { sourceType = "git",   sourcePath = "b" }, S.empty)
              , (defSR { sourceType = "darcs", sourcePath = "c" }, S.empty)
              ])
            , ("bar", GR $ M.fromList
              [ (defSR { sourceType = "git",   sourcePath = "d" }, S.empty)
              , (defSR { sourceType = "darcs", sourcePath = "e" }, S.empty)
              , (defSR { sourceType = "hg",    sourcePath = "f" }, S.empty)
              ])
            ]

      uniqueSourcesTypes db `shouldBe` S.fromList ["git", "darcs", "hg"]

    it "generates all imports" $

      T.lines (sourceImports ["git", "darcs", "hg"]) `shouldBe`
        [ "import Control.Biegunka.Source.Git"
        , "import Control.Biegunka.Source.Darcs"
        , "import Control.Biegunka.Source.Hg"
        ]

  describe "script generation" $ do

    it "generates code for groups" $

      group "foo" `shouldBe` "  group \"foo\" $ do"

    it "generates code for sources" $ do

      let sr = SR
            { sourceType   = "git"
            , sourcePath   = "/home/user/there"
            , fromLocation = "http://example.com"
            , sourceOwner  = Nothing
            }

      source "/home/user" sr `shouldBe` "    git \"http://example.com\" \"there\" $ do"

    it "generates code for files" $ do

      let fr = FR
            { fileType   = "copy"
            , filePath   = "/home/user/here"
            , fromSource = "/home/user/there/that"
            , fileOwner  = Nothing
            }

      file "/home/user" "/home/user/there" Nothing fr `shouldBe` "      copy \"that\" \"here\""

  describe "utilities" $ do

    it "correctly does hierarchical indentation" $ do

      let foo = indent groupIndent "text"
          bar = indent sourceIndent "text"
          baz = indent fileIndent "text"

          spaces = T.takeWhile isSpace

      spaces foo < spaces bar `shouldBe` True
      spaces bar < spaces baz `shouldBe` True

    it "correctly makes 'Int's and 'String's \"shown\"" $ do

      shown 7     `shouldBe` "7"
      shown "foo" `shouldBe` "\"foo\""

    it "correctly capitalizes words" $ do

      capitalize "foo"    `shouldBe` "Foo"
      capitalize "FOO"    `shouldBe` "FOO"
      capitalize "123foo" `shouldBe` "123foo"
      capitalize ""       `shouldBe` ""


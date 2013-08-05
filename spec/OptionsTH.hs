{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Monoid (mempty)
import Options.Applicative
import Test.Hspec

import Control.Biegunka.TH (biegunkaOptions)


data Environments = Default | NotSoDefault
    deriving (Show, Eq)

biegunkaOptions ''Environments


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "biegunka script command line options parser" $ do
    context "environmental" $ do
      it "handles --environmental option" $
        preview (_Right._1) (parse __opts__ ["--default"]) `shouldBe` Just Default
      it "handles different --environmental option" $
        preview (_Right._1) (parse __opts__ ["--not-so-default"]) `shouldBe` Just NotSoDefault
      it "handles --environmental options absense" $
        preview (_Right._1) (parse __opts__ [""]) `shouldBe` Nothing
      it "handles --environmental options absense" $
        preview (_Right._1) (parse __opts__ ["--run blah-blah"]) `shouldBe` Nothing
    context "running" $ do
      it "handles --run option" $
        () <$ preview (_Right._2) (parse __opts__ ["--run", "--default"]) `shouldBe` Just ()
      it "handles --dry-run option" $
        () <$ preview (_Right._2) (parse __opts__ ["--dry-run", "--not-so-default"]) `shouldBe` Just ()
      it "handles --safe-run option" $
        () <$ preview (_Right._2) (parse __opts__ ["--safe-run", "--default"]) `shouldBe` Just ()
      it "handles --check option" $
        () <$ preview (_Right._2) (parse __opts__ ["--check", "--default"]) `shouldBe` Just ()


parse :: ParserInfo a -> [String] -> Either ParserFailure a
parse parser args =
  execParserPure (prefs mempty) parser args

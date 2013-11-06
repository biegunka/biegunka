{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module THSpec where

import Control.Lens
import Data.Default.Class (def)
import Data.Monoid (mempty)
import Options.Applicative
import Test.Hspec

import Control.Biegunka.TH (biegunkaOptions)
import Control.Biegunka.Settings (mode, _Offline)


data Environments = Default | NotSoDefault deriving (Show, Eq)


biegunkaOptions ''Environments

spec :: Spec
spec =
  describe "biegunka script command line options parser" $ do
    context "settings" $
      it "handles --offline option" $
        preview (_Right._3.to ($ def).mode._Offline)
          (parse optionsParser ["--default", "--offline"]) `shouldBe` Just ()
    context "environmental" $ do
      it "handles --environmental option" $
        preview (_Right._1) (parse optionsParser ["--default"]) `shouldBe` Just Default
      it "handles different --environmental option" $
        preview (_Right._1) (parse optionsParser ["--not-so-default"]) `shouldBe` Just NotSoDefault
      it "handles --environmental options absense" $
        preview (_Right._1) (parse optionsParser [""]) `shouldBe` Nothing
      it "handles --environmental options absense" $
        preview (_Right._1) (parse optionsParser ["--run blah-blah"]) `shouldBe` Nothing
    context "running" $ do
      it "handles --run option" $
        () <$ preview (_Right._2) (parse optionsParser ["--run", "--default"]) `shouldBe` Just ()
      it "handles --dry-run option" $
        () <$ preview (_Right._2) (parse optionsParser ["--dry-run", "--not-so-default"]) `shouldBe` Just ()
      it "handles --safe-run option" $
        () <$ preview (_Right._2) (parse optionsParser ["--safe-run", "--default"]) `shouldBe` Just ()
      it "handles --check option" $
        () <$ preview (_Right._2) (parse optionsParser ["--check", "--default"]) `shouldBe` Just ()


parse :: ParserInfo a -> [String] -> Either ParserFailure a
parse = execParserPure (prefs mempty)

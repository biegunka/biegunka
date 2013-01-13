{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Biegunka
import Biegunka.Source.Dummy
import Control.Lens
import System.Directory (getHomeDirectory)
import System.Directory.Layout
import Test.Hspec


main :: IO ()
main = do
  as <- trivial_script `resultsIn` trivial_layout
  bs <- trivial_repo "biegunka-core-test" `resultsIn` trivial_layout
  cs <- simple_repo_0 `resultsIn` simple_layout_0
  ds <- trivial_repo "biegunka-core-simple0" `resultsIn` trivial_layout
  hspec $ do
    describe "Trivial biegunka script" $ do
      it "should be trivial layout too" $ null as
    describe "Trivial biegunka profile script" $ do
      it "should be trivial layout too" $ null bs
    describe "Simple biegunka profile script" $ do
      it "should be simple layout too" $ null cs
      it "should disappear after deletion" $ null ds


resultsIn :: Script Profile () -> Layout -> IO [DLCheckFailure]
resultsIn s l = do
  executeWith (defaultExecution & react .~ Ignorant) s
  fp <- getHomeDirectory
  check l fp


trivial_script :: Script Profile ()
trivial_script = return ()


trivial_layout :: Layout
trivial_layout = return ()


trivial_repo :: String -> Script Profile ()
trivial_repo p = profile p $ return ()


simple_repo_0 :: Script Profile ()
simple_repo_0 =
  profile "biegunka-core-simple0" $
    dummy l "tmp/dummies/biegunka-core-simple0" $
      copy "src0" "tmp/results/dst0"
 where
  l = file "src0" "thisiscontents\n"


simple_layout_0 :: Layout
simple_layout_0 = directory "tmp/results" $ file "dst0" "thisiscontents\n"

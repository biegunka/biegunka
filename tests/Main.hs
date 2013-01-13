{-# LANGUAGE DataKinds #-}
module Main (main) where

import Biegunka
import System.Directory (getHomeDirectory)
import System.Directory.Layout
import Test.Hspec


main :: IO ()
main = do
  as <- trivial_script `resultsIn` trivial_layout
  bs <- trivial_repo "biegunka-core-test" `resultsIn` trivial_layout
  hspec $ do
    describe "Trivial biegunka script" $ do
      it "should be trivial layout too" $ null as
    describe "Trivial biegunka profile script" $ do
      it "should be trivial layout too" $ null bs


resultsIn :: Script Profile () -> Layout -> IO [DLCheckFailure]
resultsIn s l = do
  execute s
  fp <- getHomeDirectory
  check l fp


trivial_script :: Script Profile ()
trivial_script = return ()


trivial_repo :: String -> Script Profile ()
trivial_repo p = profile p $ return ()


trivial_layout :: Layout
trivial_layout = return ()

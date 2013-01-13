{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Exception (bracket)

import Biegunka
import System.FilePath ((</>))
import System.Directory
  ( createDirectory, removeDirectoryRecursive
  , getHomeDirectory
  , getCurrentDirectory, setCurrentDirectory
  )
import System.Directory.Layout
import Test.Hspec


main :: IO ()
main = do
  xs <- expect trivial_repo trivial_layout
  hspec $ do
    describe "Trivial biegunka script" $ do
      it "should be trivial layout too" $ null xs


expect :: Script Profile () -> Layout -> IO [DLCheckFailure]
expect s l = do
  execute s
  fp <- getHomeDirectory
  check l fp


trivial_repo :: Script Profile ()
trivial_repo = return ()


trivial_layout :: Layout
trivial_layout = return ()

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Monad (forM_, unless)
import Data.Function (on)

import           Control.Monad.Free (Free(..))
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (removeDirectoryRecursive, removeFile)

import           Biegunka.DB (Biegunka(..), load, save)
import           Biegunka.Profile (Profile)
import qualified Biegunka.Interpreter.Execute.Profile as Profile


execute ∷ Free (Profile a) b → IO ()
execute script = do
  Biegunka α ← load
  β ← Profile.execute script
  removeOrphanFiles α β
  removeOrphanRepos α β
  save $ Biegunka β
 where
  removeOrphanFiles = removeOrphan removeFile files
  removeOrphanRepos = removeOrphan removeDirectoryRecursive repos

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) $ f x

  files α = M.elems α >>= M.elems >>= S.toList
  repos α = M.elems α >>= M.keys

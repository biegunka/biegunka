{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Data.Monoid (mempty)
import Control.Monad (forM_, unless, when)
import Data.Function (on)

import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (getHomeDirectory, removeDirectoryRecursive, removeFile)

import           Biegunka.DB (Biegunka(..), load, save)
import           Biegunka.DSL (ProfileScript)
import qualified Biegunka.Interpreter.Execute.Profile as Profile
import qualified Biegunka.Interpreter.ConstructMap as Map
import           Biegunka.Interpreter.State


execute ∷ ProfileScript () → IO ()
execute script = do
  home ← getHomeDirectory
  let script' = infect home script
  Biegunka α ← load
  when (α == mempty) $
    putStrLn "Warning: Biegunka is empty"
  Profile.execute script'
  let β = Map.construct script'
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

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Data.Monoid (mempty)
import Control.Monad (forM_, unless, when)
import Data.Function (on)

import           Control.Monad.Free (Free)
import           Control.Monad.State (StateT, evalStateT)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (getHomeDirectory, removeDirectoryRecursive, removeFile)

import           Biegunka.State
import           Biegunka.DB (Biegunka(..), load, save)
import           Biegunka.DSL.Profile (Profile)
import qualified Biegunka.Interpreter.Execute.Profile as Profile
import qualified Biegunka.Interpreter.ConstructMap as Map


execute ∷ StateT BiegunkaState (Free Profile) () → IO ()
execute script = do
  home ← getHomeDirectory
  let state = BiegunkaState { _root = home, _repositoryRoot = ""}
      script' = evalStateT script state
  Biegunka α ← load
  when (α == mempty) $
    putStrLn "Warning: Biegunka is empty"
  Profile.execute state script'
  let β = Map.construct state script'
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

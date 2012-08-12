{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Monad (forM_, unless)

import           Control.Monad.Free (Free(..))
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (removeFile)

import           Biegunka.DB (Biegunka(..), load, save)
import           Biegunka.Profile (Profile)
import qualified Biegunka.Interpreter.Execute.Profile as Profile


execute ∷ Free (Profile a) b → IO ()
execute script = do
  biegunka ← Profile.execute script
  Biegunka old ← load
  let oldies = M.elems old >>= M.elems >>= S.toList
      files = M.elems biegunka >>= M.elems >>= S.toList
  forM_ oldies $ \oldie →
    unless (oldie `elem` files) (removeFile oldie)
  save $ Biegunka biegunka

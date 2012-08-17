{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Log
  ( install, uninstall
  ) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Monad.Free (Free(..))
import           Control.Monad.State (evalStateT)
import           Control.Monad.Writer (execWriter, tell)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           System.FilePath ((</>))

import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Repository (Repository(..))
import Biegunka.DSL.Files (Files(..))


install ∷ Free (Profile ()) () → String
install (Free (Profile name script next)) = mconcat
  ["Setup profile ", name, "\n", profile (evalStateT script ()), "\n", install next]
install (Pure _) = ""


profile ∷ Free (Repository ()) () → String
profile (Free (Git url path script next)) = mconcat
  ["Setup repository ", url, " at ", path, "\n", repo path (evalStateT script ()), "\n", profile next]
profile (Pure _) = ""


repo ∷ FilePath → Free Files () → String
repo path (Free (Message m x)) = mconcat ["Message: ", show m, "\n", repo path x]
repo path (Free (RegisterAt dst x)) = mconcat ["Link repository ", path, " to ~/", dst, "\n", repo path x]
repo path (Free (Link src dst x)) = mconcat ["Link file ", path </> src, " to ~/", dst, "\n", repo path x]
repo path (Free (Copy src dst x)) = mconcat ["Copy file ", path </> src, " to ~/", dst, "\n", repo path x]
repo path (Free (Compile cmp src dst x)) = mconcat
  ["Compile with ", show cmp, " file ", path </> src, " to ~/", dst, "\n", repo path x]
repo _ (Pure _) = ""


uninstall ∷ Map String (Map FilePath (Set FilePath)) -> Map String (Map FilePath (Set FilePath)) → String
uninstall α β = (logNotElems `on` files) α β ++ (logNotElems `on` repos) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " ++ x ++ "\n"))
  files m = M.elems m >>= M.elems >>= S.toList
  repos m = M.elems m >>= M.keys

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

import Biegunka.State
import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Repository (Repository(..))
import Biegunka.DSL.Files (Files(..))


install ∷ BiegunkaState → Free (Profile ()) () → String
install state (Free (Profile name script next)) = mconcat
  ["Setup profile ", name, "\n", profile state (evalStateT script state), "\n", install state next]
install _ (Pure _) = ""


profile ∷ BiegunkaState → Free (Repository ()) () → String
profile state (Free (Git url path script next)) = mconcat
  ["Setup repository ", url, " at ", path, "\n", repo (evalStateT script state { _repositoryRoot = path }), "\n", profile state next]
profile _ (Pure _) = ""


repo ∷ Free Files () → String
repo (Free (Message m x)) = mconcat ["Message: ", show m, "\n", repo x]
repo (Free (RegisterAt src dst x)) = mconcat ["Link repository ", src, " to ", dst, "\n", repo x]
repo (Free (Link src dst x)) = mconcat ["Link file ", src, " to ", dst, "\n", repo x]
repo (Free (Copy src dst x)) = mconcat ["Copy file ", src, " to ", dst, "\n", repo x]
repo (Free (Compile cmp src dst x)) = mconcat
  ["Compile with ", show cmp, " file ", src, " to ", dst, "\n", repo x]
repo (Pure _) = ""


uninstall ∷ Map String (Map FilePath (Set FilePath)) → Map String (Map FilePath (Set FilePath)) → String
uninstall α β = (logNotElems `on` files) α β ++ (logNotElems `on` repos) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " ++ x ++ "\n"))
  files m = M.elems m >>= M.elems >>= S.toList
  repos m = M.elems m >>= M.keys

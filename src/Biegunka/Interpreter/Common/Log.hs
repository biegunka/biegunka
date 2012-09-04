{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.Log
  ( install, uninstall
  ) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (execWriter, tell)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.DSL (Profile(..), Source(..), Files(..))


install ∷ Free (Profile (Free (Source (Free Files ())) ())) ()
        → String
install (Free (Profile name script next)) = mconcat
  ["Setup profile ", name, "\n", profile script, install next]
install (Pure _) = ""


profile ∷ Free (Source (Free Files ())) ()
        → String
profile (Free (Git url path script next)) = mconcat
  [indent 2, "Setup repository ", url, " at ", path, "\n", repo script, profile next]
profile (Pure _) = ""


repo ∷ Free Files () → String
repo (Free (Message m x)) = mconcat [indent 4, "Message: ", show m, "\n", repo x]
repo (Free (RegisterAt src dst x)) = mconcat [indent 4, "Link repository ", src, " to ", dst, "\n", repo x]
repo (Free (Link src dst x)) = mconcat [indent 4, "Link file ", src, " to ", dst, "\n", repo x]
repo (Free (Copy src dst x)) = mconcat [indent 4, "Copy file ", src, " to ", dst, "\n", repo x]
repo (Free (Compile cmp src dst x)) = mconcat
  [indent 4, "Compile with ", show cmp, " file ", src, " to ", dst, "\n", repo x]
repo (Pure _) = ""


uninstall ∷ Map String (Map FilePath (Set FilePath)) → Map String (Map FilePath (Set FilePath)) → String
uninstall α β = (logNotElems `on` files) α β ++ (logNotElems `on` repos) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " ++ x ++ "\n"))
  files m = M.elems m >>= M.elems >>= S.toList
  repos m = M.elems m >>= M.keys


indent ∷ Int → String
indent n = replicate n ' '

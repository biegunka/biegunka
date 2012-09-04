{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.Log (install, uninstall) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Monoid (mconcat)

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (execWriter, tell)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.DSL (Profile(..), Source(..), Files(..), mfoldie)


install ∷ Free (Profile (Free (Source (Free Files ())) ())) () → String
install = profile


profile ∷ Free (Profile (Free (Source (Free Files ())) ())) () → String
profile s = mfoldie s f
 where
  f (Profile name s' _) = mconcat ["Setup profile ", name, "\n", source s']


source ∷ Free (Source (Free Files ())) () → String
source s = mfoldie s f
 where
  f (Git url path s' _) = mconcat [indent 2,"Setup repository ",url," at ",path,"\n",files s']


files ∷ Free Files () → String
files s = mfoldie s f
 where
  f (Message m _) = mconcat [indent 4,"Message: ",show m,"\n"]
  f (RegisterAt src dst _) = mconcat [indent 4,"Link repository ",src," to ",dst,"\n"]
  f (Link src dst _) = mconcat [indent 4,"Link file ",src," to ",dst,"\n"]
  f (Copy src dst _) = mconcat [indent 4,"Copy file ",src," to ",dst,"\n"]
  f (Compile cmp src dst _) = mconcat [indent 4,"Compile with ",show cmp," file ",src," to ",dst,"\n"]


indent ∷ Int → String
indent n = replicate n ' '


uninstall ∷ Map String (Map FilePath (Set FilePath)) → Map String (Map FilePath (Set FilePath)) → String
uninstall α β = (logNotElems `on` files') α β ++ (logNotElems `on` repos) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " ++ x ++ "\n"))
  files' m = M.elems m >>= M.elems >>= S.toList
  repos m = M.elems m >>= M.keys

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.Log (install, uninstall) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Monoid (mconcat)

import Control.Lens ((^.))
import Control.Monad.Free (Free(..))
import Control.Monad.Writer (execWriter, tell)

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.DSL
  ( Profile(..)
  , Source, from, to, script
  , Files(..)
  , mfoldie
  )


install ∷ Free (Profile (Free (Source (Free Files ())) ())) () → String
install = profile


profile ∷ Free (Profile (Free (Source (Free Files ())) ())) () → String
profile = mfoldie f
 where
  f (Profile name s _) = mconcat ["Setup profile ", name, "\n", source s]


source ∷ Free (Source (Free Files ())) () → String
source = mfoldie f
 where
  f s = mconcat [indent 2, "Setup repository ", s^.from, " at ", s^.to, "\n", files (s^.script)]


files ∷ Free Files () → String
files = mfoldie f
 where
  f (Message m _) = mconcat [indent 4,"Message: ",show m,"\n"]
  f (RegisterAt src dst _) = mconcat [indent 4,"Link repository ",src," to ",dst,"\n"]
  f (Link src dst _) = mconcat [indent 4,"Link file ",src," to ",dst,"\n"]
  f (Copy src dst _) = mconcat [indent 4,"Copy file ",src," to ",dst,"\n"]
  f (Compile cmp src dst _) = mconcat [indent 4,"Compile with ",show cmp," file ",src," to ",dst,"\n"]


indent ∷ Int → String
indent n = replicate n ' '


uninstall ∷ Biegunka → Biegunka → String
uninstall α β = (logNotElems `on` filepaths) α β ++ (logNotElems `on` sources) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " ++ x ++ "\n"))

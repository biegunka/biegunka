{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Pretend (pretend) where

import Data.Monoid (mconcat)

import Control.Monad.Free (Free(..))
import System.FilePath ((</>))

import Biegunka.Profile (Profile(..))
import Biegunka.Repository (Repository(..))
import Biegunka.Script (Script(..))


pretend ∷ Free (Profile a) b → String
pretend (Free (Profile name script next)) = mconcat
  ["Setup profile ", name, "\n", profile script, "\n", pretend next]
pretend (Pure _) = ""


profile ∷ Free (Repository a) b → String
profile (Free (Git url path script next)) = mconcat
  ["Setup repository ", url, " at ", path, "\n", repo script, "\n", profile next]
 where
  repo (Free (Message m x)) = mconcat ["Message: ", show m, "\n", repo x]
  repo (Free (RegisterAt dst x)) = mconcat ["Link repository ", path, " to ~/", dst, "\n", repo x]
  repo (Free (Link src dst x)) = mconcat ["Link file ", path </> src, " to ~/", dst, "\n", repo x]
  repo (Free (Copy src dst x)) = mconcat ["Copy file ", path </> src, " to ~/", dst, "\n", repo x]
  repo (Free (Compile cmp src dst x)) = mconcat
    ["Compile with ", show cmp, " file ", path </> src, " to ~/", dst, "\n", repo x]
  repo (Pure _) = ""
profile (Pure _) = ""

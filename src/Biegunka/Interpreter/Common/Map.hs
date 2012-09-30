{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.Map (construct) where

import Data.Monoid (Monoid(..))

import           Control.Lens ((^.), view)
import           Control.Monad.Free (Free(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.DB (Biegunka, biegunize)
import Biegunka.DSL
  ( Layer(..), Command(..), Action(..)
  , action, to, script
  , foldie, mfoldie
  )


construct ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → Biegunka
construct = biegunize . profile


profile ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → Map String (Map FilePath (Set FilePath))
profile = foldie ($) mempty f
 where
  f ∷ Command Profile (Free (Command Source (Free (Command Files ()) ())) ()) a
    → Map String (Map FilePath (Set FilePath))
    → Map String (Map FilePath (Set FilePath))
  f (P name s _) = M.insertWith' mappend name (source s)
  f (W p _) = f p


source ∷ Free (Command Source (Free (Command Files ()) ())) () → Map FilePath (Set FilePath)
source = mfoldie f
 where
  f s = M.singleton (s^.to) (files $ s^.script)


files ∷ Free (Command Files ()) () → Set FilePath
files = mfoldie (f . view action)
 where
  f (Message _) = mempty
  f (RegisterAt _ dst) = S.singleton dst
  f (Link _ dst) = S.singleton dst
  f (Copy _ dst) = S.singleton dst
  f (Compile _ _ dst) = S.singleton dst
  f (Template _ dst _) = S.singleton dst
  f (Mode {}) = mempty
  f (Ownership {}) = mempty

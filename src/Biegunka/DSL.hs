{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL
  ( module B
  , FileScript, SourceScript, ProfileScript
  , Next(..), foldie, mfoldie
  ) where

import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT)

import Biegunka.State as B
import Biegunka.DSL.Files as B
import Biegunka.DSL.Source as B
import Biegunka.DSL.Profile as B


type Script s α β = StateT (BiegunkaState s) (Free α) β
type FileScript s α = Script s Files α
type SourceScript s α = Script s (Source (FileScript s ())) α
type ProfileScript s α = Script s (Profile (SourceScript s ())) α


class Next f where
  next ∷ f a → a


instance Next Files where
  next (Message _ x) = x
  next (RegisterAt _ _ x) = x
  next (Link _ _ x) = x
  next (Copy _ _ x) = x
  next (Compile _ _ _ x) = x


instance Next (Source a) where
  next (Git _ _ _ x) = x


instance Next (Profile a) where
  next (Profile _ _ x) = x


foldie ∷ Next f ⇒ (a → b → b) → b → (f (Free f c) → a) → (Free f c) → b
foldie f a g (Free t) = f (g t) (foldie f a g (next t))
foldie _ a _ (Pure _) = a


mfoldie ∷ (Monoid m, Next f) ⇒ (f (Free f c) → m) → (Free f c) → m
mfoldie = foldie mappend mempty

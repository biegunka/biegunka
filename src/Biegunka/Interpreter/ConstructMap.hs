{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.ConstructMap (construct) where

import Data.Monoid (Monoid(..))

import           Control.Monad.Free (Free(..))
import           Control.Monad.State (evalStateT)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.State
import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Repository (Repository(..))
import Biegunka.DSL.Files (Files(..))


construct ∷ BiegunkaState → Free Profile () → Map String (Map FilePath (Set FilePath))
construct state (Free (Profile name script next)) =
  M.insertWith' mappend name (profile state (evalStateT script state)) (construct state next)
construct _ (Pure _) = mempty


profile ∷ BiegunkaState → Free Repository () → Map FilePath (Set FilePath)
profile state (Free (Git _ path script next)) = M.singleton path (repo (evalStateT script state)) `mappend` profile state next
profile _ (Pure _) = mempty


repo ∷ Free Files () → Set FilePath
repo (Free (Message _ x)) = repo x
repo (Free (RegisterAt _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Link _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Copy _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Compile _ _ dst x)) = S.singleton dst `mappend` repo x
repo (Pure _) = S.empty

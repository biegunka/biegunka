{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.ConstructMap (construct) where

import Data.Monoid (Monoid(..))

import           Control.Monad.Free (Free(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.DSL (Profile(..), Source(..), Files(..))


construct ∷ Free (Profile (Free (Source (Free Files ())) ())) ()
          → Map String (Map FilePath (Set FilePath))
construct (Free (Profile name script next)) =
  M.insertWith' mappend name (profile script) (construct next)
construct (Pure _) = mempty


profile ∷ Free (Source (Free Files ())) ()
        → Map FilePath (Set FilePath)
profile (Free (Git _ path script next)) = M.singleton path (repo script ) `mappend` profile next
profile (Pure _) = mempty


repo ∷ Free Files () → Set FilePath
repo (Free (Message _ x)) = repo x
repo (Free (RegisterAt _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Link _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Copy _ dst x)) = S.singleton dst `mappend` repo x
repo (Free (Compile _ _ dst x)) = S.singleton dst `mappend` repo x
repo (Pure _) = S.empty

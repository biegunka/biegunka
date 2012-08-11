{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Profile (execute) where

import qualified Data.Map as M
import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Data.Map (Map)
import Data.Set (Set)

import Biegunka.Profile (Profile(..))
import qualified Biegunka.Interpreter.Execute.Repository as Repository


execute ∷ Free (Profile a) b → IO (Map String (Map FilePath (Set FilePath)))
execute (Free (Profile name repo next)) =
  do α ← Repository.execute repo
     β ← execute next
     return $ M.insertWith' mappend name α β
execute (Pure _) = return mempty

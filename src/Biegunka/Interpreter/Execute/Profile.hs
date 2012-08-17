{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Profile (execute) where

import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))

import Biegunka.DSL.Profile (Profile(..))
import qualified Biegunka.Interpreter.Execute.Repository as Repository


execute ∷ Free (Profile a) b → IO ()
execute (Free (Profile _ repo next)) =
  do Repository.execute repo
     execute next
execute (Pure _) = return mempty

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Profile (execute) where

import Control.Monad.Free (Free(..))
import Control.Monad.State (evalStateT)

import Biegunka.State
import Biegunka.DSL.Profile (Profile(..))
import qualified Biegunka.Interpreter.Execute.Repository as Repository


execute ∷ BiegunkaState → Free Profile () → IO ()
execute state (Free (Profile _ repo next)) =
  do Repository.execute state (evalStateT repo state)
     execute state next
execute _ (Pure _) = return ()

{-# LANGUAGE DataKinds #-}
-- |
-- Interpreters tests
--
-- Checks you /can/ combine interpreters nicely
module Interpreters where

import           Control.Biegunka
import qualified Data.Monoid as M
import qualified Data.Semigroup as S


interpreter_0 :: Interpreter
interpreter_0 = confirm M.<> run

interpreter_1 :: Interpreter
interpreter_1 = confirm S.<> run

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL
  ( module B
  , Script
  ) where

import Control.Monad.Free (Free)
import Control.Monad.State (StateT)

import Biegunka.State as B
import Biegunka.DSL.Files as B
import Biegunka.DSL.Repository as B
import Biegunka.DSL.Profile as B


type Script a b = StateT BiegunkaState (Free a) b

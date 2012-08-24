{-# OPTIONS_HADDOCK hide #-}
module Biegunka.DSL
  ( module B
  , FileScript, SourceScript, ProfileScript
  ) where

import Control.Monad.Free (Free)
import Control.Monad.State (StateT)

import Biegunka.State as B
import Biegunka.DSL.Files as B
import Biegunka.DSL.Source as B
import Biegunka.DSL.Profile as B


type Script α β = StateT BiegunkaState (Free α) β
type FileScript α = Script Files α
type SourceScript α = Script (Source (FileScript ())) α
type ProfileScript α = Script (Profile (SourceScript ())) α

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


type Script s α β = StateT (BiegunkaState s) (Free α) β
type FileScript s α = Script s Files α
type SourceScript s α = Script s (Source (FileScript s ())) α
type ProfileScript s α = Script s (Profile (SourceScript s ())) α

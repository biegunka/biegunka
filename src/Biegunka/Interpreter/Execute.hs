{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Applicative ((<$>))

import Control.Monad.Free (Free(..))

import Biegunka.DB (Biegunka(..))
import Biegunka.Profile (Profile)
import qualified Biegunka.Interpreter.Execute.Profile as Profile


execute ∷ Free (Profile a) b → IO Biegunka
execute script = Biegunka <$> Profile.execute script

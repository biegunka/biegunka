{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Profile (execute) where

import Control.Monad.Free (Free(..))

import Biegunka.DSL (Profile(..), Source(..), Files(..))
import qualified Biegunka.Interpreter.Execute.Source as Source


execute ∷ Free (Profile (Free (Source (Free Files ())) ())) ()
        → IO ()
execute (Free (Profile _ repo next)) = Source.execute repo >> execute next
execute (Pure _) = return ()

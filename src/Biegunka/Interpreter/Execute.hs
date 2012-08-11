{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Applicative ((<$>))

import Control.Monad.Free (Free(..))
import qualified Data.Map as M

import Biegunka.DB (Biegunka(..))
import Biegunka.Repository (Repository(..))
import qualified Biegunka.Interpreter.Execute.Repository as Repository


execute ∷ Free (Repository a) b → IO Biegunka
execute script = Biegunka . M.singleton "default" <$> Repository.execute script

{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Profile (execute) where

import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Data.Map (Map)
import Data.Set (Set)

import Biegunka.Profile (Profile(..))
import qualified Biegunka.Interpreter.Execute.Repository as Repository


execute ∷ Free (Profile a) b → IO (Map String (Map FilePath (Set FilePath)))
execute (Free (Profile name repo next)) =
  do biegunka ← Repository.execute repo
     mappend (M.singleton name biegunka) <$> execute next
execute (Pure _) = return mempty

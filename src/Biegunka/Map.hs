{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Map (construct) where

import Data.Monoid (Monoid(..))

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, execState)
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S

import Biegunka.DB (Biegunka, biegunize)
import Biegunka.Language (Command(..), Action(..), foldieM_)


data Construct = Construct
  { _profile :: String
  , _source :: FilePath
  , _biegunka :: Map String (Map FilePath (Set FilePath))
  }


makeLenses ''Construct


construct :: Free (Command l ()) a -> Biegunka
construct cs = execState (foldieM_ g cs) Construct { _profile = mempty, _source = mempty, _biegunka = mempty } ^. biegunka . to biegunize


g :: Command l () (Free (Command l ()) a) -> State Construct ()
g (P name _ _) = do
  profile .= name
  biegunka . at name .= Just mempty
g (S _ s _ _ _) = do
  p <- use profile
  source .= s
  biegunka . at p . traverse . at s .= Just mempty
g (F a _) = do
  p <- use profile
  s <- use source
  biegunka . at p . traverse . at s . traverse <>= h a
 where
  h (Message _) = mempty
  h (RegisterAt _ dst) = S.singleton dst
  h (Link _ dst) = S.singleton dst
  h (Copy _ dst) = S.singleton dst
  h (Template _ dst _) = S.singleton dst
  h (Shell {}) = mempty
g (W _ _) = return ()

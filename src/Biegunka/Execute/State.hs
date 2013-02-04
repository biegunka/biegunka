{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.Execute.State where

import Control.Lens
import Data.Default
import Text.StringTemplate (ToSElem(..))

import Biegunka.Execute.Narrator
import Biegunka.Language (React(..))


-- | 'Execution' state.
-- Denotes current failure reaction, effective user id, used templates and more
data ExecutionState = ExecutionState
  { _priviledges :: Priviledges
  , _react       :: React
  , _reactStack  :: [React]
  , _templates   :: Templates
  , _userStack   :: [String]
  , _volubility  :: Volubility
  }


-- | Priviledges control.
-- Controls how to behave if started with sudo
data Priviledges =
    Drop     -- ^ Drop priviledges
  | Preserve -- ^ Preserve priviledges
    deriving (Show, Read, Eq, Ord)


-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. (ToSElem t) => Templates t


instance Default ExecutionState where
  def = ExecutionState
    { _priviledges = Preserve
    , _react       = Asking
    , _reactStack  = []
    , _templates   = Templates False
    , _userStack   = []
    , _volubility  = Casual
    }

makeLenses ''ExecutionState

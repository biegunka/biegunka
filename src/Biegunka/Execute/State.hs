{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.Execute.State where

import Control.Applicative
import Control.Concurrent.Chan (Chan)

import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans (MonadIO)
import Data.Default
import Text.StringTemplate (ToSElem(..))

import Biegunka.Language (React(..))


newtype Execution s a =
    E { runE :: StateT ES IO a }
    deriving (Functor, Applicative, Monad, MonadState ES, MonadIO)


-- | 'Execution' state.
-- Denotes current failure reaction, effective user id and more
data ES = ES
  { _reactStack  :: [React]
  , _userStack   :: [String]
  } deriving (Show, Read, Eq, Ord)

instance Default ES where
  def = ES
    { _reactStack  = []
    , _userStack   = []
    }

makeLenses ''ES


-- | 'Execution' environment.
-- Denotes default failure reaction, templates used and more
data EE = EE
  { _priviledges :: Priviledges
  , _react       :: React
  , _templates   :: Templates
  , _volubility  :: Volubility
  , _narrative   :: Maybe Narrative
  }

-- | Priviledges control.
-- Controls how to behave if started with sudo
data Priviledges =
    Drop     -- ^ Drop priviledges
  | Preserve -- ^ Preserve priviledges
    deriving (Show, Read, Eq, Ord)

-- | Narrator volubility: how verbose are her reports?
data Volubility =
    Talkative -- ^ Says everything you told her
  | Casual    -- ^ Casual narrator
  | Taciturn  -- ^ Doesn't say anything
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Narrative = Chan Statement

-- | Statement thoroughness
data Statement =
    Thorough { text :: String } -- ^ Highly verbose statement with lots of details
  | Typical  { text :: String } -- ^ Typical report with minimum information
    deriving (Show, Read, Eq, Ord)

-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. (ToSElem t) => Templates t

instance Default EE where
  def = EE
    { _priviledges = Preserve
    , _react       = Asking
    , _templates   = Templates ()
    , _volubility  = Casual
    , _narrative   = Nothing
    }

makeLenses ''EE

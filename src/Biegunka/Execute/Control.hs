{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.Execute.Control
  ( -- * Execution facade type
    Execution
    -- * Execution thread state
  , ES(..), reactStack, usersStack, retryCount
    -- * Execution environment
  , EE(..)
  , priviledges, react, templates, retries
  , stm, work, running, sudoing, controls, repos
    -- * Misc
  , Templates(..), Priviledges(..), Work(..)
  ) where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Lens
import Control.Monad.State (StateT)
import Data.Default
import Data.Functor.Trans.Tagged
import Data.Set (Set)
import Text.StringTemplate (ToSElem(..))

import Biegunka.Language (React(..))
import Biegunka.Control (Controls)


type Execution s a = TaggedT s (StateT ES IO) a


-- | 'Execution' thread state.
-- Denotes current failure reaction, effective user id and more
data ES = ES
  { _reactStack  :: [React]
  , _usersStack  :: [String]
  , _retryCount  :: Int
  } deriving (Show, Read, Eq, Ord)

instance Default ES where
  def = ES
    { _reactStack = []
    , _usersStack = []
    , _retryCount = 0
    }

makeLenses ''ES


-- | Concurrent parts of 'EE'
data STM = STM
  { _work    :: TQueue Work -- ^ Task queue
  , _sudoing :: TVar Bool -- ^ Whether sudoed operation is in progress.
  , _running :: TVar Bool -- ^ Whether any operation is in progress.
  , _repos   :: TVar (Set String) -- ^ Already updated repositories
  }

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come
  | Stop       -- ^ Task is done

makeLenses ''STM

-- | 'Execution' environment.
-- Denotes default failure reaction, templates used and more
data EE = EE
  { _priviledges :: Priviledges
  , _react       :: React
  , _templates   :: Templates
  , _retries     :: Int
  , _controls    :: Controls
  , _stm         :: STM
  }

-- | Priviledges control.
-- Controls how to behave if started with sudo
data Priviledges =
    Drop     -- ^ Drop priviledges
  | Preserve -- ^ Preserve priviledges
    deriving (Show, Read, Eq, Ord)

-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. ToSElem t => Templates t

makeLenses ''EE

instance Default EE where
  def = EE
    { _priviledges = Preserve
    , _react       = Ignorant
    , _templates   = Templates ()
    , _retries     = 1
    , _controls    = def
    , _stm         = undefined -- User doesn't have a chance to get there
    }

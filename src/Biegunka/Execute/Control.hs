{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.Execute.Control
  ( -- * Execution facade type
    Execution(..)
    -- * Execution thread state
  , ES(..), reactStack, usersStack, retryCount
    -- * Execution environment
  , EE(..)
  , priviledges, react, templates
  , work, order, retries, running, sudoing, controls
    -- * Misc
  , Narrative, Statement(..), Templates(..), Priviledges(..), Work(..), Order(..)
  ) where

import Control.Applicative
import Control.Concurrent.Chan (Chan)
import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans (MonadIO)
import Data.Default
import Text.StringTemplate (ToSElem(..))

import Biegunka.Language (React(..))
import Biegunka.Control (Controls)


newtype Execution s a =
    E { runE :: StateT ES IO a }
    deriving (Functor, Applicative, Monad, MonadState ES, MonadIO)


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


-- | 'Execution' environment.
-- Denotes default failure reaction, templates used and more
data EE = EE
  { _priviledges :: Priviledges
  , _react       :: React
  , _templates   :: Templates
  , _work        :: Chan Work
  , _retries     :: Int
  , _order       :: Order
  , _running     :: TVar Bool
  , _sudoing     :: TVar Bool
  , _controls    :: Controls
  }

-- | Priviledges control.
-- Controls how to behave if started with sudo
data Priviledges =
    Drop     -- ^ Drop priviledges
  | Preserve -- ^ Preserve priviledges
    deriving (Show, Read, Eq, Ord)

type Narrative = Chan Statement

-- | Statement thoroughness
data Statement =
    Thorough { text :: String } -- ^ Highly verbose statement with lots of details
  | Typical  { text :: String } -- ^ Typical report with minimum information
    deriving (Show, Read, Eq, Ord)

-- | Wrapper for templates to not have to specify `t' type on 'ExecutionState'
-- Existence of that wrapper is what made 'Default' instance possible
data Templates = forall t. ToSElem t => Templates t

-- | Workload
data Work =
    Do (IO ()) -- ^ Task to come
  | Stop       -- ^ Task is done

-- | Tasks execution order
data Order =
    Sequential -- ^ Do all tasks sequentially
  | Concurrent -- ^ Do all tasks concurrently

-- | Execution context TVar. True if sudoed operation is in progress.
sudo :: TVar Bool
sudo = unsafePerformIO $ newTVarIO False
{-# NOINLINE sudo #-}

-- | Execution context TVar. True if simple operation is in progress.
run :: TVar Bool
run = unsafePerformIO $ newTVarIO False
{-# NOINLINE run #-}


instance Default EE where
  def = EE
    { _priviledges = Preserve
    , _react       = Ignorant
    , _templates   = Templates ()
    , _work        = undefined    -- User doesn't have a chance to get there
    , _order       = Sequential
    , _retries     = 1
    , _running     = run
    , _sudoing     = sudo
    , _controls    = def
    }

makeLenses ''EE

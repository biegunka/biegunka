{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Controlling biegunka interpreters and their composition
module Control.Biegunka.Settings
  ( -- * Settings common for all interpreters
    Settings
  , HasSettings(..)
  , HasRunRoot(..)
  , defaultSettings
  , logger
  , Templates(..)
    -- ** Script targets controls
  , Targets(..)
    -- ** Biegunka mode
  , Mode(..)
  , _Online
  , _Offline
  ) where

import Control.Applicative (Applicative)
import Control.Lens
import Data.Set (Set)

import Control.Biegunka.Logger (Logger, HasLogger(..))
import Control.Biegunka.Script (HasRunRoot(..))
import Control.Biegunka.Templates
import Control.Biegunka.Templates.HStringTemplate


-- | Settings common for all interpreters and also specific for this one
data Settings = Settings
  { _runRoot      :: FilePath     -- ^ Root path for 'Source' layer
  , _biegunkaRoot :: FilePath     -- ^ Absolute of the Biegunka data files root
  , __logger      :: Maybe Logger -- ^ 'Logger' handle
  , _targets      :: Targets      -- ^ Namespaces to focus on
  , _templates    :: Templates    -- ^ Templates mapping
  , _mode         :: Mode         -- ^ Biegunka mode
  }

class HasSettings t where
  settings :: Lens' t Settings

  _logger :: Lens' t (Maybe Logger)
  _logger = settings . \f x -> f (__logger x) <&> \y -> x { __logger = y }

  targets :: Lens' t Targets
  targets = settings . \f x -> f (_targets x) <&> \y -> x { _targets = y }

  templates :: Lens' t Templates
  templates = settings . \f x -> f (_templates x) <&> \y -> x { _templates = y }

  mode :: Lens' t Mode
  mode = settings . \f x -> f (_mode x) <&> \y -> x { _mode = y }

  biegunkaRoot :: Lens' t FilePath
  biegunkaRoot = settings . \f x -> f (_biegunkaRoot x) <&> \y -> x { _biegunkaRoot = y }

instance HasSettings Settings where
  settings = id
  {-# INLINE settings #-}

instance HasRunRoot Settings where
  runRoot f x = f (_runRoot x) <&> \y -> x { _runRoot = y }

instance HasLogger Applicative Settings where
  logger = _logger.traverse

defaultSettings :: Settings
defaultSettings = Settings
  { _runRoot      = "~"
  , _biegunkaRoot = "~/.biegunka"
  , __logger      = Nothing
  , _targets      = All
  , _templates    = hStringTemplate ()
  , _mode         = defaultMode
  }

-- | Namespaces to focus on
data Targets =
    All                     -- All namespaces
  | Subset   (Set FilePath) -- The subset of namespaces
  | Children (Set FilePath) -- All children of the subset of namespaces
    deriving (Show, Eq)

data Mode = Offline | Online
    deriving (Show, Eq)

_Offline :: Prism' Mode ()
_Offline = prism' (\_ -> Offline) (\case Offline -> Just (); Online -> Nothing)
{-# ANN _Offline "HLint: ignore Use const" #-}

_Online :: Prism' Mode ()
_Online = prism' (\_ -> Online) (\case Online -> Just (); Offline -> Nothing)
{-# ANN _Online "HLint: ignore Use const" #-}

defaultMode :: Mode
defaultMode = Online

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Control.Biegunka.Settings
  ( -- * Settings common for all interpreters
    Settings
  , defaultSettings
  , HasRunRoot(..)
  , biegunkaRoot
  , logger
  , _logger
  , targets
  , local
  , templates
  , Templates(..)
    -- ** Script targets controls
  , Targets(..)
    -- ** Biegunka mode
  , mode, Mode(..), _Online, _Offline
  ) where

import Control.Lens
import Data.Default.Class (Default(..))
import Data.Set (Set)

import Control.Biegunka.Logger (Logger, HasLogger(..))
import Control.Biegunka.Script (HasRunRoot(..))
import Control.Biegunka.Templates
import Control.Biegunka.Templates.HStringTemplate


-- | Settings common for all interpreters and also specific for this one
data Settings a = Settings
  { __runRoot     :: FilePath     -- ^ Root path for 'Source' layer
  , _biegunkaRoot :: FilePath     -- ^ Absolute of the Biegunka data files root
  , __logger      :: Maybe Logger -- ^ 'Logger' handle
  , _targets      :: Targets      -- ^ Namespaces to focus on
  , _local        :: a            -- ^ Interpreter specific settings
  , _templates    :: Templates    -- ^ Templates mapping
  , _mode         :: Mode
  }

-- | Namespaces to focus on
data Targets =
    All                     -- All namespaces
  | Subset   (Set FilePath) -- The subset of namespaces
  | Children (Set FilePath) -- All children of the subset of namespaces
    deriving (Show, Read)

data Mode = Offline | Online deriving (Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''Settings

instance HasRunRoot (Settings a) where
  runRoot = _runRoot

-- |
_runRoot :: Lens' (Settings a) FilePath

-- | Absolute path of the Biegunka data files root
biegunkaRoot :: Lens' (Settings a) FilePath

instance HasLogger (Settings a) where
  logger = _logger.traverse

-- |
_logger :: Lens' (Settings a) (Maybe Logger)

-- | Namespaces to focus on
targets :: Lens' (Settings a) Targets

-- | Interpreter controls
local :: Lens (Settings a) (Settings b) a b

-- | Templates mapping
templates :: Lens' (Settings a) Templates

-- | Biegunka mode
mode :: Lens' (Settings a) Mode

makePrisms ''Mode

instance () ~ a => Default (Settings a) where
  def = defaultSettings

defaultSettings :: Settings ()
defaultSettings = Settings
  { __runRoot     = "~"
  , _biegunkaRoot = "~/.biegunka"
  , __logger      = Nothing
  , _targets      = All
  , _local        = ()
  , _templates    = hStringTemplate ()
  , _mode         = Online
  }

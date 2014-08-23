{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Control.Biegunka.Settings
  ( -- * Settings common for all interpreters
    Settings, HasRoot(..), appData, logger, targets, colors, local, templates, Templates(..)
    -- ** Script targets controls
  , Targets(..)
    -- ** Color scheme controls
  , ColorScheme(..), noColors, actionColor, sourceColor
  , srcColor, dstColor, errorColor, retryColor
    -- ** Biegunka mode
  , mode, Mode(..), _Online, _Offline
  ) where

import Control.Lens
import Data.Default.Class (Default(..))
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen

import Control.Biegunka.Log (Logger)
import Control.Biegunka.Script (HasRoot(..))
import Control.Biegunka.Templates
import Control.Biegunka.Templates.HStringTemplate


-- | Settings common for all interpreters and also specific for this one
data Settings a = Settings
  { _appRoot   :: FilePath    -- ^ Root path for 'Source' layer
  , _appData   :: FilePath    -- ^ Biegunka profile files path
  , _logger    :: Logger     -- ^ Interpreters' logger handle
  , _targets   :: Targets     -- ^ Groups to focus on
  , _colors    :: ColorScheme -- ^ Pretty printing
  , _local     :: a           -- ^ Interpreter specific settings
  , _templates :: Templates   -- ^ Templates mapping
  , _mode      :: Mode
  }

-- | Groups to focus on
data Targets =
    All                     -- All groups
  | Subset   (Set FilePath) -- The subset of groups
  | Children (Set FilePath) -- All children of the subset of groups
    deriving (Show, Read)

-- | Colors used in logger
data ColorScheme = ColorScheme
  { _actionColor :: Doc -> Doc
  , _sourceColor :: Doc -> Doc
  , _srcColor    :: Doc -> Doc
  , _dstColor    :: Doc -> Doc
  , _errorColor  :: Doc -> Doc
  , _retryColor  :: Doc -> Doc
  }

instance Default ColorScheme where
  def = ColorScheme
    { _actionColor = green
    , _sourceColor = cyan
    , _srcColor    = yellow
    , _dstColor    = magenta
    , _errorColor  = red
    , _retryColor  = yellow
    }

-- | Disable colors
noColors :: ColorScheme
noColors = ColorScheme
  { _actionColor = id
  , _sourceColor = id
  , _srcColor    = id
  , _dstColor    = id
  , _errorColor  = id
  , _retryColor  = id
  }

data Mode = Offline | Online deriving (Show)

makeLensesWith ?? ''ColorScheme $ (lensRules & generateSignatures .~ False)

-- | Action color
actionColor :: Lens' ColorScheme (Doc -> Doc)

-- | Source color
sourceColor :: Lens' ColorScheme (Doc -> Doc)

-- | Src color
srcColor :: Lens' ColorScheme (Doc -> Doc)

-- | Dst color
dstColor :: Lens' ColorScheme (Doc -> Doc)

-- | Error color
errorColor :: Lens' ColorScheme (Doc -> Doc)

-- | Retry color
retryColor :: Lens' ColorScheme (Doc -> Doc)

makeLensesWith (lensRules & generateSignatures .~ False) ''Settings

instance HasRoot (Settings a) where
  root = appRoot

-- | Root path for 'Source' layer
appRoot :: Lens' (Settings a) FilePath

-- | Biegunka profile files
appData :: Lens' (Settings a) FilePath

-- | Logger channel
logger :: Lens' (Settings a) Logger

-- | Groups to focus on
targets :: Lens' (Settings a) Targets

-- | Pretty printing
colors :: Lens' (Settings a) ColorScheme

-- | Interpreter controls
local :: Lens (Settings a) (Settings b) a b

-- | Templates mapping
templates :: Lens' (Settings a) Templates

-- | Biegunka mode
mode :: Lens' (Settings a) Mode

makePrisms ''Mode

instance () ~ a => Default (Settings a) where
  def = Settings
    { _appRoot   = "~"
    , _appData   = "~/.biegunka"
    , _logger    = undefined -- sorry
    , _targets   = All
    , _colors    = def
    , _local     = ()
    , _templates = hStringTemplate ()
    , _mode      = Online
    }

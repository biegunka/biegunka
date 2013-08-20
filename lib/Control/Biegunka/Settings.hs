{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Control.Biegunka.Settings
  ( -- * Settings common for all interpreters
    Settings, root, appData, logger, targets, colors, local, templates, Templates(..)
    -- * Script targets controls
  , Targets(..)
    -- * Color scheme controls
  , ColorScheme(..), noColors, actionColor, sourceColor
  , srcColor, dstColor, errorColor, retryColor
  ) where

import Control.Lens
import Data.Default (Default(..))
import Data.Set (Set)
import Text.PrettyPrint.ANSI.Leijen

import Control.Biegunka.Templates
import Control.Biegunka.Templates.HStringTemplate


-- | Settings common for all interpreters and also specific for this one
data Settings a = Settings
  { _root      :: FilePath    -- ^ Root path for 'Source' layer
  , _appData   :: FilePath    -- ^ Biegunka profile files path
  , _logger    :: Doc -> IO () -- ^ Logger channel
  , _targets   :: Targets     -- ^ Groups that are mentioned in script
  , _colors    :: ColorScheme -- ^ Pretty printing
  , _local     :: a           -- ^ Interpreter specific settings
  , _templates :: Templates   -- ^ Templates mapping
  }

data Targets =
    All
  | Subset (Set FilePath)
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

makeLensesWith ?? ''ColorScheme $ (defaultRules & generateSignatures .~ False)

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

makeLensesWith (defaultRules & generateSignatures .~ False) ''Settings

-- | Root path for 'Source' layer
root :: Lens' (Settings a) FilePath

-- | Biegunka profile files
appData :: Lens' (Settings a) FilePath

-- | Logger channel
logger :: Lens' (Settings a) (Doc -> IO ())

-- | Groups mentioned in script
targets :: Lens' (Settings a) Targets

-- | Pretty printing
colors :: Lens' (Settings a) ColorScheme

-- | Interpreter controls
local :: Lens (Settings a) (Settings b) a b

-- | Templates mapping
templates :: Lens' (Settings a) Templates

instance Default a => Default (Settings a) where
  def = Settings
    { _root      = "~"
    , _appData   = "~/.biegunka"
    , _logger    = const (return ())
    , _targets   = All
    , _colors    = def
    , _local     = def
    , _templates = hStringTemplate ()
    }

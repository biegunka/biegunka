{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Settings where

import Control.Lens (makeLenses)
import Data.Default (Default(def))


-- | Script settings
data Settings s t = Settings
  { _root ∷ FilePath -- ^ Root path
  , _sourceRoot ∷ FilePath -- ^ Current source root path
  , _custom ∷ s -- ^ Custom settings needed by user
  , _template ∷ t -- ^ Custom template mappings needed by user
  }


instance (Default s, Default t) ⇒ Default (Settings s t) where
  def = Settings
    { _sourceRoot = ""
    , _root = ""
    , _custom = def
    , _template = def
    }


makeLenses ''Settings

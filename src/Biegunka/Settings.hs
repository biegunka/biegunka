{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Settings where

import Control.Lens
import Data.Default (Default(def))


-- | Script settings
data Settings s t = Settings
  { _root ∷ FilePath -- ^ Root path
  , _sourceRoot ∷ FilePath -- ^ Current source root path
  , _setting ∷ s -- ^ Custom settings needed by user
  , _template ∷ t -- ^ Custom templates needed by user
  }


instance (Default s, Default t) ⇒ Default (Settings s t) where
  def = Settings
    { _sourceRoot = def
    , _root = def
    , _setting = def
    , _template = def
    }


makeLensesWith (lensRules % generateSignatures .~ False) ''Settings


-- | Convenient lens to manipulate '_root' part of 'Settings'
root ∷ Lens (Settings s t) (Settings s t) FilePath FilePath


-- | Convenient lens to manipulate '_sourceRoot' part of 'Settings'
sourceRoot ∷ Lens (Settings s t) (Settings s t) FilePath FilePath


-- | Convenient lens to manipulate '_setting' part of 'Settings'
setting ∷ Lens (Settings s t) (Settings s' t) s s'


-- | Convenient lens to manipulate '_template' part of 'Settings'
template ∷ Lens (Settings s t) (Settings s t') t t'

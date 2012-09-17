{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Settings where

import Control.Applicative ((<$>))

import Control.Lens (Lens)
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


-- | Convenient lens to manipulate '_root' part of 'Settings'
root ∷ Lens (Settings s t) (Settings s t) FilePath FilePath
root f s@(Settings {_root = x}) = (\y → s {_root = y}) <$> f x
{-# INLINE root #-}


-- | Convenient lens to manipulate '_sourceRoot' part of 'Settings'
sourceRoot ∷ Lens (Settings s t) (Settings s t) FilePath FilePath
sourceRoot f s@(Settings {_sourceRoot = x}) = (\y → s {_sourceRoot = y}) <$> f x
{-# INLINE sourceRoot #-}


-- | Convenient lens to manipulate '_setting' part of 'Settings'
setting ∷ Lens (Settings s t) (Settings s' t) s s'
setting f s@(Settings {_setting = x}) = (\y → s {_setting = y}) <$> f x
{-# INLINE setting #-}


-- | Convenient lens to manipulate '_template' part of 'Settings'
template ∷ Lens (Settings s t) (Settings s t') t t'
template f s@(Settings {_template = x}) = (\y → s {_template = y}) <$> f x
{-# INLINE template #-}

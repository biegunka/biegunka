{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Settings where

import Control.Lens (makeLenses)
import Data.Default (Default(def))


-- | Script settings
data Settings a = Settings
  { _root ∷ FilePath -- ^ Root path
  , _sourceRoot ∷ FilePath -- ^ Current source root path
  , _custom ∷ a -- ^ Custom settings needed by user
  } deriving Show


instance Default s ⇒ Default (Settings s) where
  def = Settings
    { _sourceRoot = ""
    , _root = ""
    , _custom = def
    }


makeLenses ''Settings

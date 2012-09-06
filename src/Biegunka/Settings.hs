{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Settings where

import Control.Lens (makeLenses)
import Data.Default (Default(def))


data Settings a = Settings
  { _sourceRoot ∷ FilePath
  , _root ∷ FilePath
  , _custom ∷ a
  } deriving Show


instance Default s ⇒ Default (Settings s) where
  def = Settings
    { _sourceRoot = ""
    , _root = ""
    , _custom = def
    }


makeLenses ''Settings

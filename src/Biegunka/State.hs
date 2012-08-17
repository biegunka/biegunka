{-# LANGUAGE TemplateHaskell #-}
module Biegunka.State where

import Control.Lens (makeLenses)


data BiegunkaState = BiegunkaState
  { _repositoryRoot ∷ FilePath
  , _root ∷ FilePath
  } deriving Show


makeLenses ''BiegunkaState

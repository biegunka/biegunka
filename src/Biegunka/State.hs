{-# LANGUAGE TemplateHaskell #-}
module Biegunka.State where

import Control.Lens (makeLenses)


data BiegunkaState a = BiegunkaState
  { _repositoryRoot ∷ FilePath
  , _root ∷ FilePath
  , _custom ∷ a
  } deriving Show


makeLenses ''BiegunkaState

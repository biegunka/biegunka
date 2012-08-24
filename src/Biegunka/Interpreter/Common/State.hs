{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.State (infect) where

import Control.Monad.Free (Free(..))
import Control.Monad.State (evalStateT)

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , Profile(..), Source(..), Files(..)
  )
import Biegunka.State


infect ∷ FilePath
       → ProfileScript ()
       → Free (Profile (Free (Source (Free Files ())) ())) ()
infect home script = profile state (evalStateT script state)
 where
  state = BiegunkaState { _root = home, _repositoryRoot = ""}


profile ∷ BiegunkaState
        → Free (Profile (SourceScript ())) ()
        → Free (Profile (Free (Source (Free Files ())) ())) ()
profile state (Free (Profile name script next)) =
  Free (Profile name (repo state (evalStateT script state)) (profile state next))
profile _ (Pure m) = Pure m


repo ∷ BiegunkaState
     → Free (Source (FileScript ())) ()
     → Free (Source (Free Files ())) ()
repo state (Free (Git url path script next)) =
  Free (Git url path (evalStateT script state { _repositoryRoot = path }) (repo state next))
repo _ (Pure m) = Pure m

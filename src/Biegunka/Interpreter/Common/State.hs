{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.State (infect) where

import Control.Applicative ((<$>))


import Control.Monad.Free (Free(..), iter)
import Control.Monad.State (evalStateT, runStateT)
import Data.Default (Default(def))

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , Profile(..), Source(..), Files(..)
  )
import Biegunka.State


infect ∷ Default s
       ⇒ FilePath
       → ProfileScript s ()
       → Free (Profile (Free (Source (Free Files ())) ())) ()
infect home script =
  let mas = runStateT script BiegunkaState { _root = home, _repositoryRoot = "", _custom = def }
  in profile (iter degrade (snd <$> mas)) (fst <$> mas)
 where
  degrade (Profile _ _ a) = a


profile ∷ BiegunkaState s
        → Free (Profile (SourceScript s ())) ()
        → Free (Profile (Free (Source (Free Files ())) ())) ()
profile state (Free (Profile name script next)) =
  let mas = runStateT script state
  in Free (Profile name (repo state (fst <$> mas)) (profile (iter degrade (snd <$> mas)) next))
 where
  degrade (Git _ _ _ a) = a
profile _ (Pure m) = Pure m


repo ∷ BiegunkaState s
     → Free (Source (FileScript s ())) ()
     → Free (Source (Free Files ())) ()
repo state (Free (Git url path script next)) =
  Free (Git url path (evalStateT script state { _repositoryRoot = path }) (repo state next))
repo _ (Pure m) = Pure m

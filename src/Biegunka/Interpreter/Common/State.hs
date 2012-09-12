{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.State (infect) where

import Control.Applicative ((<$>))

import Control.Lens ((^.), set, over)
import Control.Monad.Free (Free(..), iter)
import Control.Monad.State (evalStateT, runStateT)
import Data.Default (Default(def))

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , Command(..)
  , to, script, next, next, transform
  , Profile, Source, Files
  )
import Biegunka.Settings


infect ∷ (Default s, Default t)
       ⇒ FilePath
       → ProfileScript s t ()
       → Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
infect home s =
  let mas = runStateT s (set root home def)
  in profile (iter (^. next) (snd <$> mas)) (fst <$> mas)


profile ∷ Settings s t
        → Free (Command Profile (SourceScript s t ())) ()
        → Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
profile s = transform f
 where
  f (Profile name s' n) =
    let mas = runStateT s' s
    in Profile name (source s (fst <$> mas)) (profile (iter (^. next) (snd <$> mas)) n)


source ∷ Settings s t
       → Free (Command Source (FileScript s t ())) ()
       → Free (Command Source (Free (Command Files ()) ())) ()
source s = transform f
 where
  f p = over next (source s) . over script (flip evalStateT (set sourceRoot (p^.to) s)) $ p

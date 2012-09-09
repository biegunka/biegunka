{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.State (infect) where

import Control.Applicative ((<$>))

import Control.Lens ((^.), set, over)
import Control.Monad.Free (Free(..), iter)
import Control.Monad.State (evalStateT, runStateT)
import Data.Default (Default(def))

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , Profile(..)
  , Source, to, script, step
  , Files(..)
  , next, transform
  )
import Biegunka.Settings


type Freest a b = Free (a b) ()


infect ∷ (Default s, Default t)
       ⇒ FilePath → ProfileScript s t () → Freest Profile (Freest Source (Free Files ()))
infect home s =
  let mas = runStateT s (set root home def)
  in profile (iter next (snd <$> mas)) (fst <$> mas)


profile ∷ Settings s t
        → Free (Profile (SourceScript s t ())) ()
        → Freest Profile (Freest Source (Free Files ()))
profile s = transform f
 where
  f (Profile name s' n) =
    let mas = runStateT s' s
    in Profile name (source s (fst <$> mas)) (profile (iter next (snd <$> mas)) n)



source ∷ Settings s t → Free (Source (FileScript s t ())) () → Freest Source (Free Files ())
source s = transform f
 where
  f p = over step (source s) . over script (flip evalStateT (set sourceRoot (p^.to) s)) $ p

{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.State (infect) where

import Control.Applicative ((<$>))

import Control.Lens ((^.), (.~), over)
import Control.Monad.Free (Free(..), iter)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Data.Default (Default(def))

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , Command(..)
  , to, script, next, transform
  , Profile, Source, Files
  )
import Biegunka.Settings


-- | Evaluate state monad actions to get pure free monad
--
-- First we evaluate outer layer state actions: everything around 'profile' calls. Then evaluated state is passed to 'profile' calls
infect ∷ (Default s, Default t)
       ⇒ FilePath
       → ProfileScript s t ()
       → Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
infect home scr =
  let mas = runStateT scr (root .~ home $ def)
      a' = fst <$> mas
      s' = iter (^. next) (snd <$> mas)
  in profile s' a'


-- | Evaluate state monad actions inside a profile
--
-- Gets initial state as evaluated from outer layer, appends its mutations on inner layer, passes mutated state to 'source'
--
-- Next 'profile' call gets unmodified state from outer layer
profile ∷ Settings s t
        → Free (Command Profile (SourceScript s t ())) ()
        → Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
profile s = transform $ \(Profile t scr n) →
  let mas = runStateT scr s
      a' = fst <$> mas
      s' = iter (^. next) (snd <$> mas)
  in Profile t (source s' a') (profile s n)


-- | Evaluate reader monad actions inside a source
--
-- Note: source actions cannot mutate state at all
source ∷ Settings s t
       → Free (Command Source (FileScript s t ())) ()
       → Free (Command Source (Free (Command Files ()) ())) ()
source s = transform $ \p → over next (source s) $ over script (flip runReaderT (sourceRoot .~ (p^.to) $ s)) p

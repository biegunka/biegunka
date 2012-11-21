{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Flatten (flatten) where

import Control.Monad.Free (Free(..), liftF)
import Biegunka.DSL (Script, Layer(..), Command(..))


flatten ∷ Script Profile → Free (Command l ()) ()
flatten (Free t) = g t
 where
  g (P n s x) = liftF (P n () ()) >> flatten' s >> flatten x
  g (W w x) = liftF (W w ()) >> flatten x
flatten (Pure ()) = Pure ()


flatten' ∷ Script Source → Free (Command l ()) ()
flatten' (Free t) = g t
 where
  g (S u p s f x) = liftF (S u p () f ()) >> flatten'' s >> liftF (S' ()) >> flatten' x
  g (W w x) = liftF (W w ()) >> flatten' x
flatten' (Pure ()) = Pure ()


flatten'' ∷ Script Files → Free (Command l ()) ()
flatten'' (Free t) = g t
 where
  g (F a x) = liftF (F a ()) >> flatten'' x
  g (W w x) = liftF (W w ()) >> flatten'' x
flatten'' (Pure ()) = Pure ()

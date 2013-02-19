{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Flatten (tasks) where

import Control.Monad.Free (Free(..))
import Biegunka.Language (Script, Layer(..), Command(..))


tasks :: Script Profiles -> [[Command l () ()]]
tasks p = loop [] p 0
 where
  loop _ (Pure _)         _ = []
  loop a (Free (W w x))   z = loop (a ++ [W w ()]) x (z + 1)
  loop a (Free (P n s x)) z = pool (a ++ P n () () : flatten s) x z
  pool a t                0 = a : loop [] t 0
  pool a (Free (W w x))   z = pool (a ++ [W w ()]) x (z - 1)


flatten :: Script Sources -> [Command l () ()]
flatten (Free (S t u p s f x)) = S t u p () f () : flatten' s ++ flatten x
flatten (Free (W w x))         = W w () : flatten x
flatten (Pure _)               = []


flatten' :: Script Files -> [Command l () ()]
flatten' (Free (F a x)) = F a () : flatten' x
flatten' (Free (W w x)) = W w () : flatten' x
flatten' (Pure _) = []

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Flatten (flatten) where

import Control.Monad.Free (Free(..))
import Biegunka.Language (Script, Layer(..), Command(..))


flatten :: Script Profiles -> [Command l () ()]
flatten (Free (P n s x)) = P n () () : flatten' s ++ flatten x
flatten (Free (W w x))   = W w () : flatten x
flatten (Pure _)         = []


flatten' :: Script Sources -> [Command l () ()]
flatten' (Free (S t u p s f x)) = S t u p () f () : flatten'' s ++ flatten' x
flatten' (Free (W w x))         = W w () : flatten' x
flatten' (Pure _)               = []


flatten'' :: Script Files -> [Command l () ()]
flatten'' (Free (F a x)) = F a () : flatten'' x
flatten'' (Free (W w x)) = W w () : flatten'' x
flatten'' (Pure _) = []

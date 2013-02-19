{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Flatten (tasks) where

import Data.Foldable (toList)

import           Data.Sequence (Seq)
import qualified Data.Sequence as Q
import           Control.Monad.Free (Free(..))
import           Biegunka.Language


tasks :: Script Profiles -> [[Command l () ()]]
tasks = go [] Q.empty
 where
  go :: [Command l () ()] -> Seq [Command l () ()] -> Script Profiles -> [[Command l () ()]]
  go _ q (Pure _)                         = toList q
  go a q (Free (W (User     (Just u)) x)) = go (W (User     (Just u)) () : a) q x
  go a q (Free (W (Reacting (Just r)) x)) = go (W (Reacting (Just r)) () : a) q x
  go a q (Free (W (User     Nothing)  x)) = go (drop 1 a) q x
  go a q (Free (W (Reacting Nothing)  x)) = go (drop 1 a) q x
  go a q (Free (P n s x))                 = go a (q Q.|> (reverse a ++ P n () () : flatten s)) x


flatten :: Script Sources -> [Command l () ()]
flatten (Free (S t u p s f x)) = S t u p () f () : flatten' s ++ flatten x
flatten (Free (W w x))         = W w () : flatten x
flatten (Pure _)               = []


flatten' :: Script Files -> [Command l () ()]
flatten' (Free (F a x)) = F a () : flatten' x
flatten' (Free (W w x)) = W w () : flatten' x
flatten' (Pure _) = []

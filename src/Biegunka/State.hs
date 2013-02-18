{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.State (infect) where

import Control.Applicative
import Data.Monoid (mempty)

import           Control.Lens
import           Control.Monad.State (State, evalState)
import qualified System.FilePath as F

import Biegunka.Language (Command(..), Action(..))


data Infect = Infect
  { _root :: FilePath
  , _source :: FilePath
  } deriving (Show, Read, Eq, Ord)


makeLenses ''Infect


-- | Infect free monad with state:
--
-- * Path to root
-- * Path to current source
infect :: FilePath
       -> [Command l a b]
       -> [Command l a b]
infect path cs = evalState (f cs) Infect { _root = path, _source = mempty }


f :: [Command l a b] -> State Infect [Command l a b]
f (F a x : cs) = h a >>= \b -> (F b x :) <$> f cs
 where
  h (Link s d)       = liftA2 Link (use source </> pure s) (use root </> pure d)
  h (Copy s d)       = liftA2 Copy (use source </> pure s) (use root </> pure d)
  h (Template s d t) = liftA2 (\s' d' -> Template s' d' t) (use source </> pure s) (use root </> pure d)
  h (Shell fp c)     = (\r -> (Shell (r F.</> fp) c)) <$> use source
f (S t u d s a z : cs) = do
  r <- use root
  source .= (r F.</> d)
  d' <- use root </> pure d
  (S t u d' s a z :) <$> f cs
f (P n y z : cs) = (P n y z :) <$> f cs
f (W w z : cs) = (W w z :) <$> f cs
f [] = return []


(</>) :: State Infect FilePath -> State Infect FilePath -> State Infect FilePath
(</>) = liftA2 (F.</>)

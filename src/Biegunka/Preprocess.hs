{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Biegunka.Preprocess (flatten, infect) where

import Control.Applicative
import Data.Monoid (mempty)

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, evalState)
import qualified System.FilePath as F

import Biegunka.Language.External


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
       -> [EL l a b]
       -> [EL l a b]
infect path cs = evalState (f cs) Infect { _root = path, _source = mempty }


f :: [EL l a b] -> State Infect [EL l a b]
f (EF a x : cs) = h a >>= \b -> (EF b x :) <$> f cs
 where
  h (Link s d)       = liftA2 Link (use source </> pure s) (use root </> pure d)
  h (Copy s d)       = liftA2 Copy (use source </> pure s) (use root </> pure d)
  h (Template s d t) = liftA2 (\s' d' -> Template s' d' t) (use source </> pure s) (use root </> pure d)
  h (Shell fp c)     = (\r -> (Shell (r F.</> fp) c)) <$> use source
f (ES t u d s a z : cs) = do
  r <- use root
  source .= (r F.</> d)
  d' <- use root </> pure d
  (ES t u d' s a z :) <$> f cs
f (EP n y z : cs) = (EP n y z :) <$> f cs
f (EW w z : cs) = (EW w z :) <$> f cs
f [] = return []


(</>) :: State Infect FilePath -> State Infect FilePath -> State Infect FilePath
(</>) = liftA2 (F.</>)


flatten :: Script Profiles -> [EL l () ()]
flatten (Free (EW t x))   = EW t () : flatten x
flatten (Free (EP n s x)) = EP n () () : flatten' s ++ flatten x
flatten (Pure _)          = []


flatten' :: Script Sources -> [EL l () ()]
flatten' (Free (ES t u p s f x)) = ES t u p () f () : flatten'' s ++ flatten' x
flatten' (Free (EW w x))         = EW w () : flatten' x
flatten' (Pure _)                = []


flatten'' :: Script Files -> [EL l () ()]
flatten'' (Free (EF a x)) = EF a () : flatten'' x
flatten'' (Free (EW w x)) = EW w () : flatten'' x
flatten'' (Pure _)        = []

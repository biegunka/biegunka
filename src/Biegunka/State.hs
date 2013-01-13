{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.State (infect) where

import Control.Applicative
import Data.Monoid (mempty)

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, evalState)
import qualified System.FilePath as F

import Biegunka.Language (Command(..), Action(..))


data Infect = Infect
  { _root :: FilePath
  , _source :: FilePath
  }


makeLenses ''Infect


-- | Infect free monad with state:
--
-- * Path to root
-- * Path to current source
infect :: FilePath
       -> Free (Command l ()) a
       -> Free (Command l ()) a
infect path cs = evalState (f cs) Infect { _root = path, _source = mempty }


f :: Free (Command l ()) a -> State Infect (Free (Command l ()) a)
f (Free t) = Free <$> g t
f (Pure x) = return (Pure x)


g :: Command l () (Free (Command l ()) a) -> State Infect (Command l () (Free (Command l ()) a))
g (F a x) = h a >>= \t -> F t <$> f x
 where
  h m@(Message _) = return m
  h (RegisterAt src dst) =
    liftA2 RegisterAt (use source </> pure src) (use root </> pure dst)
  h (Link src dst) =
    liftA2 Link (use source </> pure src) (use root </> pure dst)
  h (Copy src dst) =
    liftA2 Copy (use source </> pure src) (use root </> pure dst)
  h (Template src dst substitute) =
    liftA2 (\s d -> Template s d substitute) (use source </> pure src) (use root </> pure dst)
  h (Shell fp c) = (\r -> (Shell (r F.</> fp) c)) <$> use source
g (S url dst s update x) = do
  r <- use root
  source .= (r F.</> dst)
  liftA5 S (pure url) (use root </> pure dst) (pure s) (pure update) (f x)
g (P n () x) = P n () <$> f x
g (W w x) = W w <$> f x


(</>) :: State Infect FilePath -> State Infect FilePath -> State Infect FilePath
(</>) = liftA2 (F.</>)


liftA5 :: Applicative m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
liftA5 h a b c d e = pure h <*> a <*> b <*> c <*> d <*> e

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.State (infect) where

import Control.Applicative (Applicative, (<$>), (<*>), liftA2, liftA3, pure)
import Data.Monoid (mempty)

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, evalState)
import qualified System.FilePath as F

import Biegunka.DSL (Command(..), Action(..))


data Infect = Infect
  { _root ∷ FilePath
  , _source ∷ FilePath
  }


makeLenses ''Infect


-- | Infect free monad with state:
--
-- * Path to root
-- * Path to current source
infect ∷ FilePath
       → Free (Command l ()) ()
       → Free (Command l ()) ()
infect path cs = evalState (f cs) Infect { _root = path, _source = mempty }


f ∷ Free (Command l ()) () → State Infect (Free (Command l ()) ())
f (Free t) = Free <$> g t
f (Pure ()) = return (Pure ())


g ∷ Command l () (Free (Command l ()) ()) → State Infect (Command l () (Free (Command l ()) ()))
g (F a x) = h a >>= \t → F t <$> f x
 where
  h m@(Message _) = return m
  h (RegisterAt src dst) =
    liftA2 RegisterAt (use source </> pure src) (use root </> pure dst)
  h (Link src dst) =
    liftA2 Link (use source </> pure src) (use root </> pure dst)
  h (Copy src dst) =
    liftA2 Copy (use source </> pure src) (use root </> pure dst)
  h (Compile cmp src dst) =
    liftA2 (Compile cmp) (use source </> pure src) (use root </> pure dst)
  h (Template src dst substitute) =
    liftA2 (\s d → Template s d substitute) (use source </> pure src) (use root </> pure dst)
  h (Mode fp mode) =
    liftA2 Mode (use root </> pure fp) (pure mode)
  h (Ownership fp user group) =
    liftA3 Ownership (use root </> pure fp) (pure user) (pure group)
g (S url dst s update x) = do
  r ← use root
  source .= (r F.</> dst)
  liftA5 S (pure url) (use root </> pure dst) (pure s) (pure update) (f x)
g (S' x) = S' <$> f x
g (P n () x) = P n () <$> f x
g (W w x) = W w <$> f x


(</>) ∷ State Infect FilePath → State Infect FilePath → State Infect FilePath
(</>) = liftA2 (F.</>)


liftA5 ∷ Applicative m ⇒ (a → b → c → d → e → f) → m a → m b → m c → m d → m e → m f
liftA5 h a b c d e = pure h <*> a <*> b <*> c <*> d <*> e

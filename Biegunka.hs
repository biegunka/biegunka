{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka
  ( (-->), bzdury
  ) where

import Control.Monad.State (StateT(..), MonadIO, execStateT, liftIO, put)
import Data.Map (Map)
import Data.Monoid (Monoid(..), mconcat)
import qualified Data.Map as M

import Biegunka.Repository

newtype Biegunka a =
  Biegunka { runBiegunka ∷ StateT BiegunkaState IO a
           } deriving (Monad, MonadIO)

type BiegunkaState = Map String [FilePath]

(-->) ∷ Repository a ⇒ a → (a → Biegunka ()) → Biegunka ()
(-->) = flip ($)

instance Monoid (Biegunka ()) where
  mempty = return ()
  mappend a b = Biegunka $ do
    s ← liftIO $ execStateT (runBiegunka a) M.empty
    t ← liftIO $ execStateT (runBiegunka b) M.empty
    put (s `M.union` t)

bzdury ∷ [Biegunka ()] → Biegunka ()
bzdury = mconcat

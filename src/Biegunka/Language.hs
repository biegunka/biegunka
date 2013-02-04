{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Language
  ( Script, Layer(..)
  , Command(..), Action(..), Wrapper(..), React(..)
  , foldie, mfoldie, foldieM, foldieM_
  , next
  ) where

import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


type family Script (a :: Layer) :: *


type instance Script Files   = Free (Command Files ()) ()
type instance Script Source  = Free (Command Source (Script Files)) ()
type instance Script Profile = Free (Command Profile (Script Source)) ()


data Layer = Files | Source | Profile


data Command (l :: Layer) s a where
  F :: Action -> a -> Command l () a
  S :: String -> FilePath -> s -> (FilePath -> IO ()) -> a -> Command l s a
  P :: String -> s -> a -> Command l s a
  W :: Wrapper -> a -> Command l s a


instance Functor (Command l s) where
  fmap f (F a x)       = F a (f x)
  fmap f (S u p s h x) = S u p s h (f x)
  fmap f (P n s x)     = P n s (f x)
  fmap f (W s x)       = W s (f x)
  {-# INLINE fmap #-}


next :: Command l s a -> a
next (F _ x)       = x
next (S _ _ _ _ x) = x
next (P _ _ x)     = x
next (W _ x)       = x
{-# INLINE next #-}


data Action =
    RegisterAt FilePath FilePath
  | Link FilePath FilePath
  | Copy FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
  | Shell FilePath String


data Wrapper =
    User (Maybe String)
  | Reacting (Maybe React)


data React = Ignorant | Asking | Abortive


foldie :: (a -> b -> b) -> b -> (Command l s (Free (Command l s) c) -> a) -> (Free (Command l s) c) -> b
foldie f a g (Free t) = g t `f` foldie f a g (next t)
foldie _ a _ (Pure _) = a


mfoldie :: Monoid m => (Command l s (Free (Command l s) c) -> m) -> (Free (Command l s) c) -> m
mfoldie = foldie mappend mempty


foldieM :: Monad m => (Command l s (Free (Command l s) c) -> m a) -> Free (Command l s) c -> m ()
foldieM = foldie (>>) (return ())
{-# SPECIALIZE foldieM :: (Command l s (Free (Command l s) c) -> IO a) -> Free (Command l s) c -> IO () #-}


foldieM_ :: Monad m => (Command l s (Free (Command l s) c) -> m ()) -> Free (Command l s) c -> m ()
foldieM_ = foldie (>>) (return ())
{-# SPECIALIZE foldieM_ :: (Command l s (Free (Command l s) c) -> IO ()) -> Free (Command l s) c -> IO () #-}

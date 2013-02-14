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
  ) where

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


type family Script (a :: Layer) :: *


type instance Script Files   = Free (Command Files ()) ()
type instance Script Sources  = Free (Command Sources (Script Files)) ()
type instance Script Profiles = Free (Command Profiles (Script Sources)) ()


data Layer = Files | Sources | Profiles


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


data Action =
    RegisterAt FilePath FilePath
  | Link FilePath FilePath
  | Copy FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
  | Shell FilePath String


data Wrapper =
    User (Maybe String)
  | Reacting (Maybe React)


data React =
    Ignorant
  | Asking
  | Abortive
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

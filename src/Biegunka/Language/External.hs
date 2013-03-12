{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Specifies user side language primitives
module Biegunka.Language.External
  ( Script, Layer(..)
  , EL(..), Action(..), Wrapper(..), React(..)
  ) where

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


type family Script (a :: Layer) :: *


type instance Script Files    = Free (EL Files ()) ()
type instance Script Sources  = Free (EL Sources (Script Files)) ()
type instance Script Profiles = Free (EL Profiles (Script Sources)) ()


data Layer = Files | Sources | Profiles


data EL (l :: Layer) s a where
  F :: Action -> a -> EL l () a
  S :: String -> String -> FilePath -> s -> (FilePath -> IO ()) -> a -> EL l s a
  P :: String -> s -> a -> EL l s a
  W :: Wrapper -> a -> EL l s a


instance Functor (EL l s) where
  fmap f (F a x)         = F a (f x)
  fmap f (S t u p s h x) = S t u p s h (f x)
  fmap f (P n s x)       = P n s (f x)
  fmap f (W s x)         = W s (f x)
  {-# INLINE fmap #-}


data Action =
    Link FilePath FilePath
  | Copy FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
  | Shell FilePath String


data Wrapper =
    User (Maybe String)
  | Reacting (Maybe React)
  | Task Bool


data React =
    Ignorant
  | Abortive
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

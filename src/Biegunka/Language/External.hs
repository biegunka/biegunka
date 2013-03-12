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
  EF :: Action -> a -> EL l () a
  ES :: String -> String -> FilePath -> s -> (FilePath -> IO ()) -> a -> EL l s a
  EP :: String -> s -> a -> EL l s a
  EW :: Wrapper -> a -> EL l s a


instance Functor (EL l s) where
  fmap f (EF a x)         = EF a (f x)
  fmap f (ES t u p s h x) = ES t u p s h (f x)
  fmap f (EP n s x)       = EP n s (f x)
  fmap f (EW s x)         = EW s (f x)
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

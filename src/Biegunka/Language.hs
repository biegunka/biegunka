{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Specifies user side and library side language primitives
module Biegunka.Language
  ( Script, Layer(..)
  , EL(..), IL(..), A(..), W(..)
  , React(..)
  ) where

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


type family Script (a :: Layer) :: *


type instance Script Actions  = Free (EL Actions) ()
type instance Script Sources  = Free (EL Sources) ()
type instance Script Profiles = Free (EL Profiles) ()


data Layer = Actions | Sources | Profiles


data EL (l :: Layer) a where
  EA :: A                                                                -> a -> EL Actions a
  ES :: String -> String -> FilePath -> Script Actions -> (FilePath -> IO ()) -> a -> EL Sources a
  EP :: String                     -> Script Sources                      -> a -> EL Profiles a
  EW :: W                                                                -> a -> EL l a

instance Functor (EL l) where
  fmap f (EA a x)         = EA a (f x)
  fmap f (ES t u p s h x) = ES t u p s h (f x)
  fmap f (EP n s x)       = EP n s (f x)
  fmap f (EW s x)         = EW s (f x)
  {-# INLINE fmap #-}


data IL =
    IA A Int String String
  | IS FilePath String (IO ()) Int String String
  | IW W


data A =
    Link FilePath FilePath
  | Copy FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
  | Shell FilePath String


data W =
    User (Maybe String)
  | Reacting (Maybe React)
  | Task Bool


data React = Ignorant | Abortive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

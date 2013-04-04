{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Specifies user side and library side languages primitives
module Biegunka.Language
  ( Script, Scope(..)
  , EL(..), IL(..), A(..), W(..)
  , React(..)
  ) where

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


type family Script (a :: Scope) :: *


type instance Script Actions  = Free (EL Actions) ()
type instance Script Sources  = Free (EL Sources) ()
type instance Script Profiles = Free (EL Profiles) ()


data Scope = Actions | Sources | Profiles


data EL (sc :: Scope) a where
  EA :: A                                                                -> a -> EL Actions a
  ES :: String -> String -> FilePath -> Script Actions -> (FilePath -> IO ()) -> a -> EL Sources a
  EP :: String                     -> Script Sources                      -> a -> EL Profiles a
  EW :: W                                                                -> a -> EL sc a

instance Functor (EL sc) where
  fmap f (EA a x)         = EA a (f x)
  fmap f (ES t u p s h x) = ES t u p s h (f x)
  fmap f (EP n s x)       = EP n s (f x)
  fmap f (EW s x)         = EW s (f x)
  {-# INLINE fmap #-}


data IL =
    IA A Int Int String String
  | IS FilePath String (IO ()) String String
  | IP String
  | IW W
  | IT [IL]


data A =
    Link FilePath FilePath
  | Copy FilePath FilePath
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
  | Shell FilePath String


data W =
    User (Maybe String)
  | Reacting (Maybe React)
  | Chain


data React = Ignorant | Abortive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

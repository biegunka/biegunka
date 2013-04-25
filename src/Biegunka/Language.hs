{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-- | Specifies user side and library side languages primitives
module Biegunka.Language
  ( Scope(..), Script(..), lift
  , EL(..), IL(..), A(..), W(..)
  , React(..)
  ) where

import Control.Applicative(Applicative(..))

import Control.Monad.Free (Free(..), liftF)
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


data Scope = Actions | Sources | Profiles


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script sc a = Script { runScript :: Free (EL sc) a }

instance Functor (Script sc) where
  fmap f (Script m) = Script (fmap f m)
  {-# INLINE fmap #-}

instance Applicative (Script sc) where
  pure v = Script (pure v)
  {-# INLINE pure #-}
  Script m <*> Script n = Script (m <*> n)
  {-# INLINE (<*>) #-}

instance Monad (Script sc) where
  return v = Script (return v)
  {-# INLINE return #-}
  Script m >>= f = Script (m >>= runScript . f)
  {-# INLINE (>>=) #-}

-- | Lift DSL term to the 'Script'
lift :: EL sc a -> Script sc a
lift = Script . liftF


data EL sc a where
  EA :: A -> a -> EL Actions a
  ES :: String -> String -> FilePath -> Script Actions () -> (FilePath -> IO ()) -> a -> EL Sources a
  EP :: String -> Script Sources () -> a -> EL Profiles a
  EW :: W -> a -> EL sc a

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

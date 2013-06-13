{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
-- | Specifies user side and library side languages primitives
module Biegunka.Language
  ( Scope(..)
  , EL(..), A(..), S(..), P(..), W(..)
  , React(..)
  , peek
  ) where

import Control.Applicative((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Control.Monad.Free (Free(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


-- | External language scopes
data Scope = Actions | Sources | Profiles


-- | External language datatype. That's what user will
-- construct with combinators from "Biegunka"
data EL :: (Scope -> *) -> Scope -> * -> * where
  EP :: f Profiles -> P -> Free (EL f Sources) () -> x -> EL f Profiles x
  ES :: f Sources -> S -> Free (EL f Actions) () -> x -> EL f Sources x
  EA :: f Actions -> A -> x -> EL f Actions x
  EW :: W -> x -> EL f s x

instance Functor (EL a s) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (EL a s) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (EL a s) where
  traverse f (EP a p i x) = EP a p i <$> f x
  traverse f (ES a s i x) = ES a s i <$> f x
  traverse f (EA a z   x) = EA a z   <$> f x
  traverse f (EW   w   x) = EW   w   <$> f x
  {-# INLINE traverse #-}

peek :: EL a s x -> x
peek (EP _ _ _ x) = x
peek (ES _ _ _ x) = x
peek (EA _ _   x) = x
peek (EW   _   x) = x


-- | 'Profiles' scope data
newtype P = P
  { pname :: String -- ^ name
  } deriving (Show, Read, Eq, Ord)

-- | 'Sources' scope data
data S = S {
  -- | Source type
    stype :: String
  -- | URI where source is located
  , suri :: String
  -- | Where to emerge source on FS (relative to Biegunka root setting)
  , spath :: FilePath
  -- | How to update source
  , supdate :: (FilePath -> IO ())
  }

-- | 'Actions' scope data
data A =
    -- | Symbolic link
    Link FilePath FilePath
    -- | Verbatim copy
  | Copy FilePath FilePath
    -- | Copy with template substitutions
  | Template FilePath FilePath (forall t. ToSElem t => t -> String -> Text)
    -- | Shell command
  | Shell FilePath String

data W =
    User (Maybe String)
  | Reacting (Maybe React)
  | Chain

data React = Ignorant | Abortive | Retry
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

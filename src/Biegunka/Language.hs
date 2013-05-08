{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-- | Specifies user side and library side languages primitives
module Biegunka.Language
  ( Scope(..), Script(..), lift
  , EL(..), IL(..), A(..), S(..), P(..), W(..)
  , React(..)
  ) where

import Control.Applicative(Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Control.Monad.Free (Free(..), liftF)
import Data.Default (Default(..))
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

instance Default a => Default (Script sc a) where
  def = return def

-- | Lift DSL term to the 'Script'
lift :: EL sc a -> Script sc a
lift = Script . liftF
{-# INLINE lift #-}


-- | External language datatype. That's what user will
-- construct with combinators from "Biegunka"
data EL sc a where
  EP :: P -> Script Sources () -> a -> EL Profiles a
  ES :: S -> Script Actions () -> a -> EL Sources a
  EA :: A -> a -> EL Actions a
  EW :: W -> a -> EL sc a

instance Functor (EL sc) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (EL sc) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (EL sc) where
  traverse f (EP p i x) = EP p i <$> f x
  traverse f (ES s i x) = ES s i <$> f x
  traverse f (EA a   x) = EA a   <$> f x
  traverse f (EW w   x) = EW w   <$> f x
  {-# INLINE traverse #-}


-- | 'Profiles' scope data
newtype P = Profile
  { pname :: String -- ^ name
  } deriving (Show, Read, Eq, Ord)

-- | 'Sources' scope data
data S = Source {
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

data React = Ignorant | Abortive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


data IL =
    IA A Int Int String String
  | IS FilePath String (IO ()) String String
  | IP String
  | IW W
  | IT [IL]

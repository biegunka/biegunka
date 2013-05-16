{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Specifies user side and library side languages primitives
module Biegunka.Language
  ( Scope(..), Script(..), liftS, annotate, Annotation
  , EL(..), IL(..), A(..), S(..), P(..), W(..)
  , React(..)
  , zoom
  ) where

import Control.Applicative(Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), fmapDefault, foldMapDefault)

import Control.Monad.Free (Free(..), liftF, iter)
import Control.Monad.State (MonadState(..), StateT(..), lift)
import Data.Default (Default(..))
import Data.Text.Lazy (Text)
import Text.StringTemplate (ToSElem)
import Text.StringTemplate.GenericStandard ()


data Scope = Actions | Sources | Profiles

type family Annotation (sc :: Scope) :: *
type instance Annotation Profiles = Int
type instance Annotation Sources  = Int
type instance Annotation Actions  = ()


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script sc a = Script { runScript :: StateT Int (Free (EL (Annotation sc) sc)) a }

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

instance MonadState Int (Script sc) where
  get = Script get
  {-# INLINE get #-}
  put = Script . put
  {-# INLINE put #-}
  state = Script . state
  {-# INLINE state #-}

instance Default a => Default (Script sc a) where
  def = return def
  {-# INLINE def #-}

-- | Lift DSL term to the 'Script'
liftS :: EL (Annotation sc) sc a -> Script sc a
liftS = Script . lift . liftF
{-# INLINE liftS #-}

-- | Annotate DSL
annotate :: Script sc a -> Int -> (Free (EL (Annotation sc) sc) a, Int)
annotate i s =
  let r = runStateT (runScript i) (s + 1)
      ast = fmap fst r
      s' = iter zoom $ fmap snd r
  in (ast, s')


-- | External language datatype. That's what user will
-- construct with combinators from "Biegunka"
data EL ann sc a where
  EP :: ann -> P -> Free (EL ann' Sources) () -> a -> EL ann Profiles a
  ES :: ann -> S -> Free (EL ann' Actions) () -> a -> EL ann Sources a
  EA :: ann -> A -> a -> EL ann Actions a
  EW :: W -> a -> EL ann sc a

instance Functor (EL ann sc) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

instance Foldable (EL ann sc) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (EL ann sc) where
  traverse f (EP a p i x) = EP a p i <$> f x
  traverse f (ES a s i x) = ES a s i <$> f x
  traverse f (EA a z   x) = EA a z   <$> f x
  traverse f (EW   w   x) = EW   w   <$> f x
  {-# INLINE traverse #-}

zoom :: EL ann sc a -> a
zoom (EP _ _ _ x) = x
zoom (ES _ _ _ x) = x
zoom (EA _ _   x) = x
zoom (EW   _   x) = x


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

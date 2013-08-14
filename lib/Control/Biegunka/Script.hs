{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | Configuration script machinery
--
-- Gets interesting static information from script
-- before passing it to interpreters, which then make use of it
module Control.Biegunka.Script
  ( -- * Script types
    Script(..), Annotate(..)
    -- ** Implementation details
  , MAnnotations, Annotations
    -- * Get annotated script
  , runScript, evalScript
    -- * Script mangling
  , script, annotate, sourced, actioned, constructTargetFilePath
    -- * Lenses
  , app, profileName, sourcePath, sourceURL, profiles
  , token, order, sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, UserW(..), User(..), React(..), Retry(..), incr
  , Target(..), Into, into
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens hiding (Action)
import Control.Monad.Free (Free(..), iter, liftF)
import Control.Monad.State (StateT(..))
import Control.Monad.Reader (ReaderT(..), local)
import Control.Monad.Trans (lift)
import Data.Copointed (copoint)
import Data.Default (Default(..))
import Data.Set (Set)
import Data.String (IsString(..))
import System.FilePath ((</>))
import System.FilePath.Lens
import System.Posix.Types (CUid)
import System.Process (CmdSpec(..))

import Control.Biegunka.Language
import Control.Biegunka.QQ

-- $setup
-- >>> :set -XOverloadedStrings


-- | Language 'Term' annotations
--
-- Different scopes require different kinds of
-- annotations, so it is a family of them
data family Annotate (sc :: Scope) :: *
data instance Annotate Sources = AS
  { asToken      :: Int
  , asProfile    :: String
  , asUser       :: Maybe UserW
  , asMaxRetries :: Retry
  , asReaction   :: React
  }
data instance Annotate Actions = AA
  { aaURI        :: URI
  , aaOrder      :: Int
  , aaMaxOrder   :: Int
  , aaUser       :: Maybe UserW
  , aaMaxRetries :: Retry
  , aaReaction   :: React
  }


-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

-- | User setting modifier
data User u where
  UserID   :: CUid -> User CUid
  Username :: String -> User String

instance Show (User u)

instance u ~ String => IsString (User u) where
  fromString = Username

-- | Because I can
instance u ~ CUid => Num (User u) where
  UserID a + UserID b = UserID (a + b)
  UserID a * UserID b = UserID (a * b)
  abs (UserID a)      = UserID (abs a)
  signum (UserID a)   = signum (UserID a)
  fromInteger         = UserID . fromInteger

-- | Wrapper around 'User' hiding particular
-- implementation from the type sistem
data UserW = forall u. UserW (User u)

deriving instance Show UserW

-- | Failure reaction
--
-- Used then all retries errored
data React = Ignorant | Abortive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Retries count
newtype Retry = Retry { unRetry :: Int }
    deriving (Show, Read, Eq, Ord)

instance Default Retry where
  def = Retry def

-- | Increment retry count
incr :: Retry -> Retry
incr (Retry n) = Retry (succ n)
{-# INLINE incr #-}

-- | Script annotations environment
data Annotations = Annotations
  { _app            :: FilePath    -- ^ Biegunka root filepath
  , _profileName    :: String      -- ^ Profile name
  , _sourcePath     :: FilePath    -- ^ Source root filepath
  , _sourceURL      :: URI         -- ^ Current source url
  , _activeUser     :: Maybe UserW -- ^ Maximum action order in current source
  , _maxRetries     :: Retry       -- ^ Maximum retries count
  , _sourceReaction :: React       -- ^ How to react on source failure
  , _actionReaction :: React       -- ^ How to react on action failure
  }

deriving instance Show Annotations

instance Default Annotations where
  def = Annotations
    { _app = def
    , _profileName    = def
    , _sourcePath     = def
    , _sourceURL      = def
    , _activeUser     = def
    , _maxRetries     = Retry 1
    , _sourceReaction = Abortive
    , _actionReaction = Ignorant
    }
  {-# INLINE def #-}

-- | Script annotations state
--
-- Mnemonic is 'Mutable Annotations'
data MAnnotations = MAnnotations
  { _token    :: Int        -- ^ Unique term token
  , _profiles :: Set String -- ^ Profile name
  , _order    :: Int        -- ^ Current action order
  , _maxOrder :: Int        -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default MAnnotations where
  def = MAnnotations
    { _token    = def
    , _profiles = def
    , _order    = def
    , _maxOrder = def
    }
  {-# INLINE def #-}


-- * Lenses

makeLensesWith ?? ''Annotations   $ defaultRules & generateSignatures .~ False

-- | Biegunka filepath root
app :: Lens' Annotations FilePath

-- | Current profile name
profileName :: Lens' Annotations String

-- | Current source filepath
sourcePath :: Lens' Annotations FilePath

-- | Current source url
sourceURL :: Lens' Annotations String

-- | Current user
activeUser :: Lens' Annotations (Maybe UserW)

-- | Maximum retries count
maxRetries :: Lens' Annotations Retry

-- | How to react on source failure
sourceReaction :: Lens' Annotations React

-- | How to react on action failure
actionReaction :: Lens' Annotations React

makeLensesWith ?? ''MAnnotations $ defaultRules & generateSignatures .~ False

-- | Unique token for each 'TP'/'TS'
token :: Lens' MAnnotations Int

-- | All profiles encountered so far
profiles :: Lens' MAnnotations (Set String)

-- | Current action order
order :: Lens' MAnnotations Int

-- | Maximum action order in current source
maxOrder :: Lens' MAnnotations Int


-- | Newtype used to provide better error messages
-- for type errors in DSL (for users, mostly)
newtype Script s a = Script
  { unScript :: ReaderT Annotations
      (StateT MAnnotations (Free (Term Annotate s))) a
  }

instance Functor (Script s) where
  fmap f (Script m) = Script (fmap f m)
  {-# INLINE fmap #-}

instance Applicative (Script s) where
  pure v = Script (pure v)
  {-# INLINE pure #-}
  Script m <*> Script n = Script (m <*> n)
  {-# INLINE (<*>) #-}

instance Monad (Script s) where
  return v = Script (return v)
  {-# INLINE return #-}
  Script m >>= f = Script (m >>= unScript . f)
  {-# INLINE (>>=) #-}

instance Default a => Default (Script s a) where
  def = return def
  {-# INLINE def #-}

-- | Biegunka script shell commands
instance (scope ~ Actions, a ~ ()) => Eval (Script scope a) where
  eval command args = actioned (\_ sfp -> Command sfp (RawCommand command args))
  {-# INLINE eval #-}


-- | Get annotated DSL and resulting annotations alongside
runScript
  :: MAnnotations
  -> Annotations
  -> Script s a
  -> (Free (Term Annotate s) a, MAnnotations)
runScript s e (Script i) =
  let r       = runStateT (runReaderT i e) s
      ast     = fmap fst r
      (_, as) = iter copoint r
  in (ast, as)
{-# INLINE runScript #-}

-- | Get annotated DSL without annotations
evalScript
  :: MAnnotations
  -> Annotations
  -> Script s a
  -> Free (Term Annotate s) a
evalScript = ((fst .) .) . runScript
{-# INLINE evalScript #-}

-- | Lift DSL term to annotated 'Script'
script :: Term Annotate s a -> Script s a
script = Script . liftS
{-# INLINE script #-}

-- | Half-lift DSL term to 'Script'
liftS
  :: Term Annotate s a
  -> ReaderT Annotations (StateT MAnnotations (Free (Term Annotate s))) a
liftS = lift . liftF
{-# INLINE liftS #-}


-- | Annotate DSL
annotate
  :: Script s a
  -> ReaderT Annotations
      (StateT MAnnotations (Free (Term Annotate t))) (Free (Term Annotate s) a)
annotate i = ReaderT $ \e -> StateT $ \s -> return (runScript s e i)

-- | Abstract away all plumbing needed to make source
sourced
  :: Target p => String -> URI -> p
  -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path inner update = Script $ do
  rfp <- view app
  local (set sourcePath (constructTargetFilePath rfp url path) . set sourceURL url) $ do
    annotation <- AS
      <$> use token
      <*> view profileName
      <*> view activeUser
      <*> view maxRetries
      <*> view sourceReaction

    order    .= 0
    maxOrder .= size inner
    ast    <- annotate inner

    source <- view sourcePath

    liftS $ TS annotation (Source ty url source update) ast ()

    profiles . contains (asProfile annotation) .= True
    token += 1

-- | Get 'Actions' scoped script size measured in actions
size :: Script Actions a -> Int
size = iterFrom 0 go . evalScript def def
 where
  go :: Term Annotate Actions Int -> Int
  go (TA _ _ result) = succ result
  go (TM _ result)   = result

-- | Inline '<$' into 'iter'
--
-- > iterFrom x f = iter f . (x <$)
iterFrom :: Functor f => a -> (f a -> a) -> Free f b -> a
iterFrom zero phi = go where
  go (Pure _) = zero
  go (Free m) = phi (go <$> m)
  {-# INLINE go #-}
{-# INLINE iterFrom #-}

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script Actions ()
actioned f = Script $ do
  annotation <- AA
    <$> view sourceURL
    <*> (order <+= 1)
    <*> use maxOrder
    <*> view activeUser
    <*> view maxRetries
    <*> view actionReaction
  rfp <- view app
  sfp <- view sourcePath

  liftS $ TA annotation (f rfp sfp) ()


-- | Possible file targeting options
class Target p where
  destination :: p -> FilePath -> FilePath

instance Target FilePath where
  destination = const
  {-# INLINE destination #-}


-- | Targeting inside specified path
newtype Into a = Into { unInto :: a }
  deriving (Show, Read)

instance a ~ FilePath => Target (Into a) where
  destination p filepath = unInto p </> filepath
  {-# INLINE destination #-}

-- | Place stuff /into/ directory instead of using filename directly
into :: FilePath -> Into FilePath
into = Into
{-# INLINE into #-}

-- | Construct destination 'FilePath'
--
-- >>> constructToFilepath "" "" ""
-- ""
--
-- >>> constructTargetFilePath "/root" "from" "to"
-- "/root/to"
--
-- >>> constructTargetFilePath "/root" "from" "to/"
-- "/root/to/"
--
-- >>> constructTargetFilePath "/root" "from" (into "to")
-- "/root/to/from"
--
-- >>> constructTargetFilePath "/root" "from" "/to"
-- "/to"
--
-- >>> constructTargetFilePath "/root" "from" "/to/"
-- "/to/"
--
-- >>> constructTargetFilePath "/root" "from" (into "/to")
-- "/to/from"
constructTargetFilePath :: Target p => FilePath -> FilePath -> p -> FilePath
constructTargetFilePath root s path =
  root </> destination path (s^.filename)

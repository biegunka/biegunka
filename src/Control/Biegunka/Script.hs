{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | Configuration script machinery
--
-- Gets interesting static information from script
-- before passing it to interpreters, which then make use of it
module Control.Biegunka.Script
  ( -- * Script types
    Script(..), Annotate(..)
    -- ** Annotations
  , MAnnotations, Annotations
    -- ** Environment
  , HasRoot(..), HasSource(..)
    -- * Get annotated script
  , runScript, evalScript
    -- * Script mangling
  , script, sourced, actioned, constructTargetFilePath
    -- * Lenses
  , app, profileName, sourcePath, sourceURL, profiles
  , order, sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, UserW(..), User(..), React(..), Retry(..), incr, into
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens hiding (Action)
import Control.Monad.Free (Free(..), iter, liftF)
import Control.Monad.State (StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), local)
import Control.Monad.Trans (lift)
import Data.Copointed (copoint)
import Data.Default.Class (Default(..))
import Data.List (isSuffixOf)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Void (Void)
import System.FilePath ((</>))
import System.FilePath.Lens
import System.Command.QQ (Eval(..))
import System.Posix.Types (CUid)
import System.Process (CmdSpec(..))

import Control.Biegunka.Language
import Control.Biegunka.Script.Token

-- $setup
-- >>> :set -XOverloadedStrings


-- | Language 'Term' annotations
--
-- Different scopes require different kinds of
-- annotations, so it is a family of them
data family Annotate (sc :: Scope) :: *
data instance Annotate Sources = AS
  { asToken      :: Token
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
  def = Retry 0

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
    { _app            = mempty
    , _profileName    = mempty
    , _sourcePath     = mempty
    , _sourceURL      = mempty
    , _activeUser     = Nothing
    , _maxRetries     = Retry 1
    , _sourceReaction = Abortive
    , _actionReaction = Ignorant
    }
  {-# INLINE def #-}

-- | Script annotations state
--
-- Mnemonic is 'Mutable Annotations'
data MAnnotations = MAnnotations
  { _profiles :: Set String -- ^ Profile name
  , _order    :: Int        -- ^ Current action order
  , _maxOrder :: Int        -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default MAnnotations where
  def = MAnnotations
    { _profiles = mempty
    , _order    = 0
    , _maxOrder = 0
    }
  {-# INLINE def #-}


-- * Lenses

makeLensesWith ?? ''Annotations   $ defaultRules & generateSignatures .~ False

class HasRoot s where
  -- | Biegunka root
  root :: Lens' s FilePath

instance HasRoot Annotations where
  root = app

class HasSource s where
  -- | Source root
  source :: Lens' s FilePath

instance HasSource Annotations where
  source = sourcePath

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

-- | All profiles encountered so far
profiles :: Lens' MAnnotations (Set String)

-- | Current action order
order :: Lens' MAnnotations Int

-- | Maximum action order in current source
maxOrder :: Lens' MAnnotations Int


-- | Newtype used to provide better error messages
-- for type errors in DSL (for users, mostly)
newtype Script s a = Script
  { unScript ::
      StreamT (Tokenize s)
        (ReaderT Annotations
          (StateT MAnnotations (Free (Term Annotate s)))) a
  } deriving (Functor, Applicative, Monad, MonadReader Annotations)

instance Default a => Default (Script s a) where
  def = return def
  {-# INLINE def #-}

-- | Biegunka script shell commands
instance (scope ~ Actions, a ~ ()) => Eval (Script scope a) where
  eval command args = actioned (\_ sfp -> Command sfp (RawCommand command args))
  {-# INLINE eval #-}


type family Tokenize (s :: Scope) :: *
type instance Tokenize Actions = Void
type instance Tokenize Sources = Token


-- | Get annotated DSL and resulting annotations alongside
runScript
  :: MAnnotations
  -> Annotations
  -> Infinite (Tokenize s)
  -> Script s a
  -> (Free (Term Annotate s) a, MAnnotations)
runScript s e es (Script i) =
  let r       = runStateT (runReaderT (runStreamT es i) e) s
      ast     = fmap fst r
      (_, as) = iter copoint r
  in (ast, as)
{-# INLINE runScript #-}

-- | Get annotated DSL without annotations
evalScript
  :: MAnnotations
  -> Annotations
  -> Infinite (Tokenize s)
  -> Script s a
  -> Free (Term Annotate s) a
evalScript = (((fst .) .) .) . runScript
{-# INLINE evalScript #-}

-- | Lift DSL term to annotated 'Script'
script :: Term Annotate s a -> Script s a
script = Script . liftS
{-# INLINE script #-}

-- | Half-lift DSL term to 'Script'
liftS
  :: Term Annotate s a
  -> StreamT (Tokenize s) (ReaderT Annotations (StateT MAnnotations (Free (Term Annotate s)))) a
liftS = lift . liftF
{-# INLINE liftS #-}


-- | Annotate 'Actions' DSL
annotateActions
  :: Script Actions a
  -> StreamT Token
      (ReaderT Annotations
        (StateT MAnnotations (Free (Term Annotate Sources)))) (Free (Term Annotate Actions) a)
annotateActions i =
  lift . ReaderT $ \e -> StateT $ \s -> return (runScript s e noTokens i)

-- | Abstract away all plumbing needed to make source
sourced
  :: String -> URI -> FilePath
  -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path inner update = Script $ do
  rfp <- view app
  local (set sourcePath (constructTargetFilePath rfp url path) . set sourceURL url) $ do
    token <- next
    annotation <- AS
      <$> pure token
      <*> view profileName
      <*> view activeUser
      <*> view maxRetries
      <*> view sourceReaction

    order    .= 0
    maxOrder .= size inner
    ast    <- annotateActions inner

    sfp <- view sourcePath

    liftS $ TS annotation (Source ty url sfp update) ast ()

    profiles . contains (asProfile annotation) .= True

-- | Get 'Actions' scoped script size measured in actions
size :: Script Actions a -> Int
size = iterFrom 0 go . evalScript def def noTokens
 where
  go :: Term Annotate Actions Int -> Int
  go (TA _ _ result) = succ result
  go (TM _ result)   = result

-- | Inline '<$' into 'iter' to avoid unnecessary pass though the whole structure
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


-- | Construct destination 'FilePath'
--
-- >>> constructTargetFilePath "" "" ""
-- ""
--
-- >>> constructTargetFilePath "/root" "from" "to"
-- "/root/to"
--
-- >>> constructTargetFilePath "/root" "from" "to/"
-- "/root/to/from"
--
-- >>> constructTargetFilePath "/root" "from" (into "to")
-- "/root/to/from"
--
-- >>> constructTargetFilePath "/root" "from" "/to"
-- "/to"
--
-- >>> constructTargetFilePath "/root" "from" "/to/"
-- "/to/from"
--
-- >>> constructTargetFilePath "/root" "from" (into "/to")
-- "/to/from"
constructTargetFilePath :: FilePath -> FilePath -> FilePath -> FilePath
constructTargetFilePath r s path =
  r </> path </> case "/" `isSuffixOf` path of
    True  -> s^.filename
    False -> ""

-- | A hack to support the notion of making destination 'FilePath' inside some directory
into :: FilePath -> FilePath
into = (++ "/")
{-# INLINE into #-}
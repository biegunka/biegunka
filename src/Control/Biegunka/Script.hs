{-# LANGUAGE CPP #-}
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
  , HasRunRoot(..), HasSourceRoot(..)
    -- * Get annotated script
  , runScript, evalScript
    -- * Script mangling
  , script, sourced, actioned, constructTargetFilePath
    -- * Lenses
  , profileName, sourceURL, profiles
  , order, sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, User(..), React(..), Retry(..), incr, into
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Lens
import Control.Monad.Free (Free(..), iter, liftF)
import Control.Monad.State (StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), local)
import Control.Monad.Trans (lift)
import Data.Copointed (copoint)
import Data.Default.Class (Default(..))
import Data.List (isSuffixOf)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty)
#endif
import Data.Set (Set)
import Data.Void (Void)
import System.Command.QQ (Eval(..))
import System.Directory.Layout (User(..))
import System.FilePath ((</>))
import System.FilePath.Lens
import System.Process (CmdSpec(..))

import Control.Biegunka.Language
import Control.Biegunka.Script.Token

{-# ANN module "HLint: ignore Use if" #-}

-- $setup
-- >>> :set -XOverloadedStrings


-- | Language 'Term' annotations
--
-- Different scopes require different kinds of
-- annotations, so it is a family of them
data family Annotate (sc :: Scope) :: *
data instance Annotate 'Sources = AS
  { asToken      :: Token
  , asProfile    :: String
  , asUser       :: Maybe User
  , asMaxRetries :: Retry
  , asReaction   :: React
  }
data instance Annotate 'Actions = AA
  { aaRunRoot    :: FilePath
  , aaSourceRoot :: FilePath
  , aaURI        :: URI
  , aaOrder      :: Int
  , aaMaxOrder   :: Int
  , aaUser       :: Maybe User
  , aaMaxRetries :: Retry
  , aaReaction   :: React
  }


-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

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
  { __runRoot       :: FilePath   -- ^ Absolute path of the Source layer root
  , _profileName    :: String     -- ^ Profile name
  , __sourceRoot    :: FilePath   -- ^ Absolute path of the Action layer root
  , _sourceURL      :: URI        -- ^ Current source url
  , _activeUser     :: Maybe User -- ^ Maximum action order in current source
  , _maxRetries     :: Retry      -- ^ Maximum retries count
  , _sourceReaction :: React      -- ^ How to react on source failure
  , _actionReaction :: React      -- ^ How to react on action failure
  }

deriving instance Show Annotations

instance Default Annotations where
  def = Annotations
    { __runRoot       = mempty
    , _profileName    = mempty
    , __sourceRoot    = mempty
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

makeLensesWith ?? ''Annotations $ lensRules & generateSignatures .~ False

class HasRunRoot s where
  -- | Absolute path of the Source layer root
  runRoot :: Lens' s FilePath

instance HasRunRoot Annotations where
  runRoot = _runRoot

instance HasRunRoot (Annotate 'Actions) where
  runRoot f aa = f (aaRunRoot aa) <&> \x -> aa { aaRunRoot = x }

class HasSourceRoot s where
  -- | Absolute path of the Action layer root
  sourceRoot :: Lens' s FilePath

instance HasSourceRoot Annotations where
  sourceRoot = _sourceRoot

instance HasSourceRoot (Annotate 'Actions) where
  sourceRoot f aa = f (aaSourceRoot aa) <&> \x -> aa { aaSourceRoot = x }

_runRoot :: Lens' Annotations FilePath

-- | Current profile name
profileName :: Lens' Annotations String

_sourceRoot :: Lens' Annotations FilePath

-- | Current source url
sourceURL :: Lens' Annotations String

-- | Current user
activeUser :: Lens' Annotations (Maybe User)

-- | Maximum retries count
maxRetries :: Lens' Annotations Retry

-- | How to react on source failure
sourceReaction :: Lens' Annotations React

-- | How to react on action failure
actionReaction :: Lens' Annotations React

makeLensesWith ?? ''MAnnotations $ lensRules & generateSignatures .~ False

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
instance (scope ~ 'Actions, a ~ ()) => Eval (Script scope a) where
  eval command args = actioned (\_ sr -> Command sr (RawCommand command args))
  {-# INLINE eval #-}


type family Tokenize (s :: Scope) :: *
type instance Tokenize 'Actions = Void
type instance Tokenize 'Sources = Token


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
  :: Script 'Actions a
  -> StreamT Token
      (ReaderT Annotations
        (StateT MAnnotations (Free (Term Annotate 'Sources)))) (Free (Term Annotate 'Actions) a)
annotateActions i =
  lift . ReaderT $ \e -> StateT $ \s -> return (runScript s e noTokens i)

-- | Abstract away all plumbing needed to make source
sourced
  :: String -> URI -> FilePath
  -> Script 'Actions () -> (FilePath -> IO ()) -> Script 'Sources ()
sourced ty url path inner update = Script $ do
  rr <- view runRoot
  local (set sourceRoot (constructTargetFilePath rr url path) . set sourceURL url) $ do
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

    sr <- view sourceRoot

    liftS $ TS annotation (Source ty url sr update) ast ()

    profiles . contains (asProfile annotation) .= True

-- | Get 'Actions' scoped script size measured in actions
size :: Script 'Actions a -> Int
size = iterFrom 0 go . evalScript def def noTokens
 where
  go :: Term Annotate 'Actions Int -> Int
  go (TA _ _ result) = succ result
  go (TM _ result)   = result

-- | Inline '<$' into 'iter' to avoid unnecessary pass over the whole structure
--
-- > iterFrom x f = iter f . (x <$)
iterFrom :: Functor f => a -> (f a -> a) -> Free f b -> a
iterFrom zero phi = go where
  go (Pure _) = zero
  go (Free m) = phi (go <$> m)
  {-# INLINE go #-}
{-# INLINE iterFrom #-}

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script 'Actions ()
actioned f = Script $ do
  rr <- view runRoot
  sr <- view sourceRoot

  annotation <- AA
    <$> pure rr
    <*> pure sr
    <*> view sourceURL
    <*> (order <+= 1)
    <*> use maxOrder
    <*> view activeUser
    <*> view maxRetries
    <*> view actionReaction

  liftS $ TA annotation (f rr sr) ()


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
    True  -> view filename s
    False -> ""

-- | A hack to support the notion of making destination 'FilePath' inside some directory
into :: FilePath -> FilePath
into = (++ "/")
{-# INLINE into #-}

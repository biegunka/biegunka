{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
  , segments
  , sourceURL
  , namespaces
  , sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, User(..), React(..), Retries(..), incr, into
    -- * Namespace
  , Namespace
  , Segment
  , segmented
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
import Data.List (isSuffixOf, intercalate)
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
  , asSegments   :: [Segment]
  , asUser       :: Maybe User
  , asMaxRetries :: Retries
  , asReaction   :: React
  }
data instance Annotate 'Actions = AA
  { aaRunRoot    :: FilePath
  , aaSegments   :: [Segment]
  , aaSourceRoot :: FilePath
  , aaURI        :: URI
  , aaUser       :: Maybe User
  , aaMaxRetries :: Retries
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
newtype Retries = Retries { unRetries :: Int }
    deriving (Show, Read, Eq, Ord)

instance Default Retries where
  def = Retries 0

-- | Increment retry count
incr :: Retries -> Retries
incr (Retries n) = Retries (succ n)
{-# INLINE incr #-}

-- | Script annotations environment
data Annotations = Annotations
  { __runRoot       :: FilePath   -- ^ Absolute path of the Source layer root
  , _segments       :: [Segment]  -- ^ Namespace segments
  , __sourceRoot    :: FilePath   -- ^ Absolute path of the Action layer root
  , _sourceURL      :: URI        -- ^ Current source url
  , _activeUser     :: Maybe User -- ^ Maximum action order in current source
  , _maxRetries     :: Retries    -- ^ Maximum retries count
  , _sourceReaction :: React      -- ^ How to react on source failure
  , _actionReaction :: React      -- ^ How to react on action failure
  } deriving (Show)

instance Default Annotations where
  def = Annotations
    { __runRoot       = mempty
    , _segments       = []
    , __sourceRoot    = mempty
    , _sourceURL      = mempty
    , _activeUser     = Nothing
    , _maxRetries     = Retries 1
    , _sourceReaction = Abortive
    , _actionReaction = Ignorant
    }
  {-# INLINE def #-}

-- | Script annotations state
--
-- Mnemonic is 'Mutable Annotations'
data MAnnotations = MAnnotations
  { _namespaces :: Set [Segment] -- ^ All encountered namespaces
  } deriving (Show, Read)

instance Default MAnnotations where
  def = MAnnotations
    { _namespaces = mempty
    }
  {-# INLINE def #-}


-- * Lenses

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
_runRoot f x = f (__runRoot x) <&> \y -> x { __runRoot = y }

-- | Namespace segments in use
segments :: Lens' Annotations [String]
segments f x = f (_segments x) <&> \y -> x { _segments = y }

_sourceRoot :: Lens' Annotations FilePath
_sourceRoot f x = f (__sourceRoot x) <&> \y -> x { __sourceRoot = y }

-- | Current source url
sourceURL :: Lens' Annotations String
sourceURL f x = f (_sourceURL x) <&> \y -> x { _sourceURL = y }

-- | Current user
activeUser :: Lens' Annotations (Maybe User)
activeUser f x = f (_activeUser x) <&> \y -> x { _activeUser = y }

-- | Maximum retries count
maxRetries :: Lens' Annotations Retries
maxRetries f x = f (_maxRetries x) <&> \y -> x { _maxRetries = y }

-- | How to react on source failure
sourceReaction :: Lens' Annotations React
sourceReaction f x = f (_sourceReaction x) <&> \y -> x { _sourceReaction = y }

-- | How to react on action failure
actionReaction :: Lens' Annotations React
actionReaction f x = f (_actionReaction x) <&> \y -> x { _actionReaction = y }

-- | All namespaces encountered so far
namespaces :: Lens' MAnnotations (Set [Segment])
namespaces f x = f (_namespaces x) <&> \y -> x { _namespaces = y }


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
  -> Script 'Actions () -> (FilePath -> IO (Maybe String)) -> Script 'Sources ()
sourced ty url path inner update = Script $ do
  rr <- view runRoot
  local (set sourceRoot (constructTargetFilePath rr url path) . set sourceURL url) $ do
    token <- next
    annotation <- AS
      <$> pure token
      <*> view segments
      <*> view activeUser
      <*> view maxRetries
      <*> view sourceReaction

    ast <- annotateActions inner
    sr  <- view sourceRoot

    liftS $ TS annotation (Source ty url sr update) ast ()

    namespaces . contains (asSegments annotation) .= True

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script 'Actions ()
actioned f = Script $ do
  rr <- view runRoot
  sr <- view sourceRoot

  annotation <- AA
    <$> pure rr
    <*> view segments
    <*> pure sr
    <*> view sourceURL
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


type Namespace = String

type Segment = String

segmented :: Iso' Namespace [Segment]
segmented = iso (reverse . splitOn '/') (intercalate "/" . reverse)
 where
  splitOn e = go
    where
      go xs = case break (== e) xs of
        (y, []) -> y : []
        (y, ys) -> y : go ys

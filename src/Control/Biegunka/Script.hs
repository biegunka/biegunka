{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
-- | Configuration script machinery
--
-- Gets interesting static information from script
-- before passing it to interpreters, which then make use of it
module Control.Biegunka.Script
  ( -- * Script types
    Script(..), Annotate(..)
    -- ** Annotations
  , MAnnotations
  , defaultMAnnotations
  , Annotations
  , defaultAnnotations
    -- ** Environment
  , HasRunRoot(..), HasSourceRoot(..)
    -- * Get annotated script
  , evalScript
    -- * Script mangling
  , script, sourced, actioned, constructTargetFilePath
    -- * Lenses
  , segments
  , sourceURL
  , sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, User(..), React(..), Retries(..), defaultRetries, incr, into, peekToken
    -- * Namespace
  , Namespace
  , Segment
  , segmented
  ) where

import Control.Lens
import Control.Monad.Free (iter, liftF)
import Control.Monad.State (MonadState, StateT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), local)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import Data.List (isSuffixOf, intercalate)
import System.Command.QQ (Eval(..))
import System.Directory.Layout (User(..))
import System.FilePath ((</>))
import System.FilePath.Lens
import System.Process (CmdSpec(..))

import Control.Biegunka.Language


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

defaultRetries :: Retries
defaultRetries = Retries 0

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

defaultAnnotations :: Annotations
defaultAnnotations = Annotations
  { __runRoot       = mempty
  , _segments       = []
  , __sourceRoot    = mempty
  , _sourceURL      = mempty
  , _activeUser     = Nothing
  , _maxRetries     = Retries 1
  , _sourceReaction = Abortive
  , _actionReaction = Ignorant
  }

-- | Script annotations state
--
-- Mnemonic is 'Mutable Annotations'
data MAnnotations = MAnnotations
  { _tokens     :: Stream Token
  } deriving (Show, Eq)

defaultMAnnotations :: MAnnotations
defaultMAnnotations = MAnnotations
  { _tokens = startFrom (Token 0)
  }

-- | Token stream.
tokens :: Lens' MAnnotations (Stream Token)
tokens f x = f (_tokens x) <&> \y -> x { _tokens = y }


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


-- | Newtype used to provide better error messages
-- for type errors in DSL (for users, mostly)
newtype Script s a = Script
  { unScript ::
      ReaderT Annotations
        (StateT MAnnotations (Term Annotate s)) a
  } deriving (Functor, Applicative, Monad, MonadReader Annotations)

-- | Biegunka script shell commands
instance (scope ~ 'Actions, a ~ ()) => Eval (Script scope a) where
  eval command args = actioned (\_ sr -> Command sr (RawCommand command args))
  {-# INLINE eval #-}


data Stream a = Cons a (Stream a)
    deriving (Show, Eq)

startFrom :: Enum a => a -> Stream a
startFrom x = Cons x (startFrom (succ x))

-- | Get annotated DSL and resulting annotations alongside
runScript
  :: MAnnotations
  -> Annotations
  -> Script s a
  -> (Term Annotate s a, MAnnotations)
runScript s e (Script i) = let
    r       = runStateT (runReaderT i e) s
    ast     = fmap fst r
    (_, as) = iter nextTerm r
  in
    (ast, as)
 where
  nextTerm (TS    _ _ _ x) = x
  nextTerm (TA    _ _   x) = x
  nextTerm (TWait   _   x) = x
{-# INLINE runScript #-}

-- | Get annotated DSL without annotations
evalScript
  :: MAnnotations
  -> Annotations
  -> Script s a
  -> Term Annotate s a
evalScript = ((fst .) .) . runScript
{-# INLINE evalScript #-}

-- | Lift DSL term to annotated 'Script'
script :: TermF Annotate s a -> Script s a
script = Script . liftS
{-# INLINE script #-}

-- | Half-lift DSL term to 'Script'
liftS
  :: TermF Annotate s a
  -> ReaderT Annotations (StateT MAnnotations (Term Annotate s)) a
liftS = lift . liftF
{-# INLINE liftS #-}


-- | Annotate 'Actions' DSL
annotateActions
  :: Monad m
  => Script 'Actions a
  -> ReaderT Annotations (StateT MAnnotations m) (Term Annotate 'Actions a)
annotateActions i =
  ReaderT $ \e ->
    StateT $ \s ->
      return (runScript s e i)

-- | Abstract away all plumbing needed to make source
sourced
  :: String
  -> URI
  -> FilePath
  -> Script 'Actions ()
  -> (FilePath -> IO (Maybe String, IO (Maybe String)))
  -> Script 'Sources ()
sourced sourceType sourceUri path inner sourceUpdate = Script $ do
  rr <- view runRoot
  local (set sourceRoot (constructTargetFilePath rr sourceUri path) . set sourceURL sourceUri) $ do
    ann <- AS
      <$> nextToken
      <*> view segments
      <*> view activeUser
      <*> view maxRetries
      <*> view sourceReaction

    ast <- annotateActions inner
    sourcePath <- view sourceRoot

    liftS (TS ann Source { sourceType, sourceUri, sourcePath, sourceUpdate } ast ())

nextToken :: MonadState MAnnotations m => m Token
nextToken = do
  Cons t ts <- use tokens
  tokens .= ts
  return t

peekToken :: MonadState MAnnotations m => m Token
peekToken = do
  Cons t _ <- use tokens
  return t

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script 'Actions ()
actioned f = Script $ do
  rr <- view runRoot
  sr <- view sourceRoot

  ann <- AA
    <$> pure rr
    <*> view segments
    <*> pure sr
    <*> view sourceURL
    <*> view activeUser
    <*> view maxRetries
    <*> view actionReaction

  liftS (TA ann (f rr sr) ())


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
  r </> path </> bool "" (view filename s) ("/" `isSuffixOf` path)

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
{-# ANN segmented "HLint: ignore Use list literal" #-}

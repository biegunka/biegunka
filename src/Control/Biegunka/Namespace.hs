{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Namespace data.
--
-- Uses acid-state to store data before script invocations.
--
-- Interpreters can modify (create\/update\/delete) data any way they want:
--
--   * First, interpreter must call 'open' to get data handle
--
--   * Next, interpreter changes 'these' and call 'commit'
--
--   * Last, interpreter says 'close'
module Control.Biegunka.Namespace
  ( Db, Namespaces(..), NamespaceRecord(..), SourceRecord(..), FileRecord(..)
  , HasNamespaces(namespaces)
  , namespacing
  , withDb
  , commit
  , fromScript
  , diff, files, sources
  , segmented
  ) where

import           Control.Applicative
import           Control.Exception (bracket)
import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad ((<=<))
import           Control.Monad.Free (iterM)
import           Control.Monad.Reader (asks)
import           Control.Monad.State (State, execState, modify)
import           Data.Acid
import           Data.Acid.Local
import           Data.Function (on)
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.SafeCopy (deriveSafeCopy, base, extension, Migrate(..))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Prelude hiding (any, elem)
import           System.FilePath.Lens hiding (extension)

import           Control.Biegunka.Settings (Settings, biegunkaRoot)
import           Control.Biegunka.Language (Scope(..), Term, TermF(..), Source(Source), File(..), origin, path)
import           Control.Biegunka.Script (Annotate(..), segmented)


data SourceRecord_v0 = SR_v0 String FilePath FilePath

-- | Source data record
data SourceRecord = SR
  { sourceType   :: String                    -- 'Source' type: @git@\/@darcs@\/etc.
  , fromLocation :: FilePath                  -- 'Source' location (url, basically)
  , sourcePath   :: FilePath                  -- Path to 'Source'
  , sourceOwner  :: Maybe (Either String Int) -- 'Source' owner
  } deriving (Show, Eq)

-- | Only destination filepath matters for ordering
--
-- FIXME: Use a newtype.
instance Ord SourceRecord where
  (<=) = (<=) `on` sourcePath

deriveSafeCopy 0 'base ''SourceRecord_v0

instance Migrate SourceRecord where
  type MigrateFrom SourceRecord = SourceRecord_v0
  migrate (SR_v0 t s p) = SR t s p Nothing

deriveSafeCopy 1 'extension ''SourceRecord


data FileRecord_v0 = FR_v0 String FilePath FilePath
  deriving (Show, Read)

-- | File data record
data FileRecord = FR
  { fileType   :: String                    -- File type: @link@\/@copy@\/etc.
  , fromSource :: FilePath                  -- File source location (path on disk)
  , filePath   :: FilePath                  -- Path to file
  , fileOwner  :: Maybe (Either String Int) -- File owner
  } deriving (Show, Eq)

-- | Only destination filepath matters for ordering
instance Ord FileRecord where
  (<=) = (<=) `on` filePath

deriveSafeCopy 0 'base ''FileRecord_v0

instance Migrate FileRecord where
  type MigrateFrom FileRecord = FileRecord_v0
  migrate (FR_v0 t s p) = FR t s p Nothing

deriveSafeCopy 1 'extension ''FileRecord


-- | Namespace data record
--
-- A mapping from 'Source' data to a set of 'FileRecord's
newtype NamespaceRecord = NR
  { unGR :: Map SourceRecord (Set FileRecord)
  } deriving (Show, Eq, Typeable)

instance Monoid NamespaceRecord where
  mempty = NR mempty
  NR a `mappend` NR b = NR (a `mappend` b)

type instance Index NamespaceRecord = SourceRecord
type instance IxValue NamespaceRecord = Set FileRecord

instance Ixed NamespaceRecord where
  ix k f (NR x) = NR <$> ix k f x

deriveSafeCopy 0 'base ''NamespaceRecord


-- | All namespaces data
--
-- A mapping from namespace names to 'NamespaceRecord's
newtype Namespaces = Namespaces { _unNamespaces :: Map String NamespaceRecord }
    deriving (Show, Typeable)

type instance Index Namespaces = String
type instance IxValue Namespaces = NamespaceRecord

instance Ixed Namespaces where
  ix k = namespacing.ix k
  {-# INLINE ix #-}

instance At Namespaces where
  at k = namespacing.at k
  {-# INLINE at #-}

instance Monoid Namespaces where
  mempty = Namespaces mempty
  Namespaces xs `mappend` Namespaces ys = Namespaces (xs `mappend` ys)

class HasNamespaces a where
  namespaces :: Lens' a Namespaces

instance HasNamespaces Namespaces where
  namespaces = id
  {-# INLINE namespaces #-}

namespacing :: Iso' Namespaces (Map String NamespaceRecord)
namespacing = iso _unNamespaces Namespaces

deriveSafeCopy 0 'base ''Namespaces

getMapping :: Query Namespaces (Map String NamespaceRecord)
getMapping = asks _unNamespaces

putMapping :: Map String NamespaceRecord -> Update Namespaces ()
putMapping y = modify (\x -> x { _unNamespaces = y })

makeAcidic ''Namespaces ['getMapping, 'putMapping]


-- | Namespace data state
--
-- Consists of acid-state handle and two parts: relevant namespaces
-- and irrelevant namespaces
--
-- Relevant namespaces (or 'these') are namespaces mentioned in script so
-- interpreter won't deal with others (or 'those')
data Db = Db
  { _commit     :: Namespaces -> IO ()
  , _close      :: IO ()
  , _namespaces :: Namespaces
  }

instance HasNamespaces Db where
  namespaces = (\f x -> f (_namespaces x) <&> \y -> x { _namespaces = y }).namespaces
  {-# INLINE namespaces #-}

withDb :: Settings -> (Db -> IO a) -> IO a
withDb s = bracket (open s) close

-- | Open namespace data from disk
--
-- Searches @'appData'\/groups@ path for namespace data. Starts empty
-- if nothing is found
open :: Settings -> IO Db
open settings = do
  let (fp, _) = settings & biegunkaRoot <</>~ "groups"
  acid <- openLocalStateFrom fp mempty
  gs   <- query acid GetMapping
  return Db
    { _commit = \db -> update acid (PutMapping (_unNamespaces db))
    , _close = createCheckpointAndClose acid
    , _namespaces = Namespaces gs
    }
{-# ANN open ("HLint: ignore Avoid lambda" :: String) #-}

-- | Update namespace data
--
-- Combines 'these' and 'those' to get full state
commit :: Db -> Namespaces -> IO ()
commit = _commit

-- | Save namespace data to disk
close :: Db -> IO ()
close = _close

-- | Get namespace difference
diff :: Eq b => (a -> [b]) -> a -> a -> [b]
diff f = (\\) `on` f

-- | Get all destination filepaths in 'Namespaces'
files :: Namespaces -> [FileRecord]
files = S.elems <=< M.elems . unGR <=< M.elems . _unNamespaces

-- | Get all sources location in 'Namespaces'
sources :: Namespaces -> [SourceRecord]
sources = M.keys . unGR <=< M.elems . _unNamespaces


-- | Extract namespace data from script
--
-- Won't get /all/ mentioned namespaces but only those for which there is
-- some useful action to do.
fromScript :: Term Annotate 'Sources a -> Namespaces
fromScript script = execState (iterM construct script) (Namespaces mempty)
 where
  construct :: TermF Annotate 'Sources (State Namespaces a) -> State Namespaces a
  construct term = case term of
    TS (AS { asSegments }) (Source sourceType fromLocation sourcePath _) i next -> do
      let record = SR { sourceType, fromLocation, sourcePath, sourceOwner = Nothing }
          namespace = view (from segmented) asSegments
      at namespace . non mempty <>= NR (M.singleton record mempty)
      iterM (populate namespace record) i
      next
    TW _ next -> next

  populate
    :: String                                       -- ^ Namespace
    -> SourceRecord                                 -- ^ Source info record
    -> TermF Annotate 'Actions (State Namespaces a) -- ^ Current script term
    -> State Namespaces a
  populate ns source term = case term of
    TF _ action next -> do
      assign (ix ns.ix source.contains (record action Nothing)) True
      next
    TC _ _ next -> next
    TW _ next -> next
   where
    record :: File t FilePath FilePath -> Maybe (Either String Int) -> FileRecord
    record a fileOwner = let
        fromSource = view origin a
        filePath = view path a
        fileType = case a of
          FC {} -> "copy"
          FT {} -> "template"
          FL {} -> "link"
      in
        FR { fileType, fromSource, filePath, fileOwner }

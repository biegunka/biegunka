{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ( Partitioned, Namespaces, NamespaceRecord(..), SourceRecord(..), FileRecord(..)
  , these, those, namespaces
  , open, commit, close, fromScript
  , diff, files, sources
  , who
  , segmented
  ) where

import           Control.Applicative
import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad ((<=<))
import           Control.Monad.Free (Free(..), iterM)
import           Control.Monad.State (State, execState)
import           Data.Acid
import           Data.Acid.Local
import           Data.Aeson
import           Data.Foldable (any, elem, for_)
import           Data.Function (on)
import           Data.List ((\\))
import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.SafeCopy (deriveSafeCopy, base, extension, Migrate(..))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Prelude hiding (any, elem)
import           System.FilePath.Lens hiding (extension)

import Control.Biegunka.Settings
  (Settings, Targets(..), biegunkaRoot, targets)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..), segmented, User(..), User(..))

-- $setup
-- >>> import Data.Default


who :: Maybe (Either String Int) -> String
who = either id show . fromMaybe (Left "(unknown)")


data SourceRecord_v0 = SR_v0 String FilePath FilePath

-- | Source data record
data SourceRecord = SR
  { sourceType   :: String                    -- 'Source' type: @git@\/@darcs@\/etc.
  , fromLocation :: FilePath                  -- 'Source' location (url, basically)
  , sourcePath   :: FilePath                  -- Path to 'Source'
  , sourceOwner  :: Maybe (Either String Int) -- 'Source' owner
  } deriving (Show, Read)

-- | Only destination filepath matters for equality
instance Eq SourceRecord where
  (==) = (==) `on` sourcePath

-- | Only destination filepath matters for ordering
instance Ord SourceRecord where
  (<=) = (<=) `on` sourcePath

instance ToJSON SourceRecord where
  toJSON SR { sourceType, fromLocation, sourcePath, sourceOwner } = object
    [ "type" .= sourceType
    , "from" .= fromLocation
    , "path" .= sourcePath
    , "user" .= who sourceOwner
    ]

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
  } deriving (Show)

-- | Only destination filepath matters for equality
instance Eq FileRecord where
  (==) = (==) `on` filePath

-- | Only destination filepath matters for ordering
instance Ord FileRecord where
  (<=) = (<=) `on` filePath

instance ToJSON FileRecord where
  toJSON FR { fileType, fromSource, filePath, fileOwner } = object
    [ "type" .= fileType
    , "from" .= fromSource
    , "path" .= filePath
    , "user" .= who fileOwner
    ]

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

instance ToJSON NamespaceRecord where
  toJSON (NR t) = object [             "sources" .= map repo   (M.toList t)]
   where
    repo (k, v) = object ["info" .= k, "files"   .= map toJSON (S.toList v)]

deriveSafeCopy 0 'base ''NamespaceRecord


-- | All namespaces data
--
-- A mapping from namespace names to 'NamespaceRecord's
newtype Namespaces = Namespaces { _namespaces :: Map String NamespaceRecord }
    deriving (Show, Typeable)

instance ToJSON Namespaces where
  toJSON (Namespaces gs) = object [ "groups" .= toJSON gs ]

instance Monoid Namespaces where
  mempty = Namespaces mempty
  Namespaces xs `mappend` Namespaces ys = Namespaces (xs `mappend` ys)

makeLensesWith (lensRules & generateSignatures .~ False) ''Namespaces

-- | All namespace data
namespaces :: Lens' Namespaces (Map String NamespaceRecord)

deriveSafeCopy 0 'base ''Namespaces

getMapping :: Query Namespaces (Map String NamespaceRecord)
getMapping = view namespaces

putMapping :: Map String NamespaceRecord -> Update Namespaces ()
putMapping = assign namespaces

makeAcidic ''Namespaces ['getMapping, 'putMapping]


-- | Namespace data state
--
-- Consists of acid-state handle and two parts: relevant namespaces
-- and irrelevant namespaces
--
-- Relevant namespaces (or 'these') are namespaces mentioned in script so
-- interpreter won't deal with others (or 'those')
data Partitioned a = Partitioned
  { _acidic :: AcidState a -- ^ The namespace database handle
  , _these  :: a           -- ^ Namespaces targeted by the script
  , _those  :: a           -- ^ All other namespaces
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Partitioned

-- | Namespace database handle
acidic :: Lens' (Partitioned a) (AcidState a)

-- | Namespaces targeted by the script
these :: Lens' (Partitioned a) a

-- | All other namespaces
those :: Lens' (Partitioned a) a


-- | Open namespace data from disk
--
-- Searches @'appData'\/groups@ path for namespace data. Starts empty
-- if nothing is found
open :: Settings -> IO (Partitioned Namespaces)
open settings = do
  let (path, _) = settings & biegunkaRoot <</>~ "groups"
  acid <- openLocalStateFrom path mempty
  gs   <- query acid GetMapping
  let (xs, ys) = mentioned (partition (view targets settings)) gs
  return Partitioned { _acidic = acid, _these = xs, _those = ys }
 where
  partition All          = \_ _ -> True
  partition (Subset s)   = \k _ -> k `elem` s
  partition (Children s) = \k _ -> any (`isChildOf` k) s
   where
    isChildOf x y = x `elem` directories y
    directories   = toListOf (takingWhile (/= ".") (iterated (view directory)))

  mentioned p gs = let (xs, ys) = M.partitionWithKey p gs in (Namespaces xs, Namespaces ys)

-- | Update namespace data
--
-- Combines 'these' and 'those' to get full state
commit :: Partitioned Namespaces -> IO ()
commit db = update (view acidic db) (PutMapping (M.union (view (those.namespaces) db) (view (these.namespaces) db)))

-- | Save namespace data to disk
close :: Partitioned Namespaces -> IO ()
close = createCheckpointAndClose . view acidic

-- | Get namespace difference
diff :: Eq b => (a -> [b]) -> a -> a -> [b]
diff f = (\\) `on` f

-- | Get all destination filepaths in 'Namespaces'
files :: Namespaces -> [FilePath]
files = map filePath . S.elems <=< M.elems . unGR <=< M.elems . view namespaces

-- | Get all sources location in 'Namespaces'
sources :: Namespaces -> [FilePath]
sources = map sourcePath . M.keys . unGR <=< M.elems . view namespaces


-- | Extract namespace data from script
--
-- Won't get /all/ mentioned namespaces but only those for which there is
-- some useful action to do.
fromScript :: Free (Term Annotate 'Sources) a -> Namespaces
fromScript script = execState (iterM construct script) (Namespaces mempty)
 where
  construct :: Term Annotate 'Sources (State Namespaces a) -> State Namespaces a
  construct term = case term of
    TS (AS { asSegments, asUser }) (Source sourceType fromLocation sourcePath _) i next -> do
      let record = SR { sourceType, fromLocation, sourcePath, sourceOwner = fmap user asUser }
          namespace = view (from segmented) asSegments
      namespaces . at namespace . non mempty <>= NR (M.singleton record mempty)
      iterM (populate namespace record) i
      next
    TWait _ next -> next

  populate
    :: String                                      -- ^ Namespace
    -> SourceRecord                                -- ^ Source info record
    -> Term Annotate 'Actions (State Namespaces a) -- ^ Current script term
    -> State Namespaces a
  populate ns source term = case term of
    TA (AA { aaUser }) action next -> do
      for_ (toRecord action (fmap user aaUser)) $ \record ->
        assign (namespaces.ix ns.ix source.contains record) True
      next
    TWait _ next -> next
   where
    toRecord (Link src dst)     = toFileRecord "link" src dst
    toRecord (Copy src dst)     = toFileRecord "copy" src dst
    toRecord (Template src dst) = toFileRecord "template" src dst
    toRecord (Command {})       = const Nothing

    toFileRecord fileType fromSource filePath fileOwner =
      Just FR { fileType, fromSource, filePath, fileOwner }

user :: User -> Either String Int
user (Username s) = Left s
user (UserID n) = Right (fromIntegral n)

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
-- | Groups data control
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
module Control.Biegunka.Groups
  ( Partitioned, Groups, GroupRecord(..), SourceRecord(..), FileRecord(..)
  , these, those, groups
  , open, commit, close, fromScript
  , diff, files, sources
  , who
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
  (Settings, Targets(..), appData, targets)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..), User(..), User(..))

-- $setup
-- >>> import Data.Default


who :: Maybe (Either String Int) -> String
who = either id show . maybe (Left "(unknown)") id


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


-- | Group data record
--
-- A mapping from 'Source' data to set of 'FileRecord's
newtype GroupRecord = GR
  { unGR :: Map SourceRecord (Set FileRecord)
  } deriving (Show, Eq, Typeable)

instance Monoid GroupRecord where
  mempty = GR mempty
  GR a `mappend` GR b = GR (a `mappend` b)

type instance Index GroupRecord = SourceRecord
type instance IxValue GroupRecord = Set FileRecord

instance Ixed GroupRecord where
  ix k f (GR x) = GR <$> ix k f x

instance ToJSON GroupRecord where
  toJSON (GR t) = object [             "sources" .= map repo   (M.toList t)]
   where
    repo (k, v) = object ["info" .= k, "files"   .= map toJSON (S.toList v)]

deriveSafeCopy 0 'base ''GroupRecord


-- | All groups data
--
-- A mapping from group names to 'GroupRecord's
newtype Groups = Groups { _groups :: Map String GroupRecord }
    deriving (Show, Typeable)

instance ToJSON Groups where
  toJSON (Groups gs) = object [ "groups" .= toJSON gs ]

instance Monoid Groups where
  mempty = Groups mempty
  Groups xs `mappend` Groups ys = Groups (xs `mappend` ys)

makeLensesWith (lensRules & generateSignatures .~ False) ''Groups

-- | All groups data
groups :: Lens' Groups (Map String GroupRecord)

deriveSafeCopy 0 'base ''Groups

getMapping :: Query Groups (Map String GroupRecord)
getMapping = view groups

putMapping :: Map String GroupRecord -> Update Groups ()
putMapping = assign groups

makeAcidic ''Groups ['getMapping, 'putMapping]


-- | Groups data state
--
-- Consists of acid-state handle and two parts: relevant groups
-- and irrelevant groups
--
-- Relevant groups (or 'these') are groups mentioned in script so
-- interpreter won't deal with others (or 'those')
data Partitioned a = Partitioned
  { _acidic :: AcidState a -- ^ The groups database handle
  , _these  :: a           -- ^ Groups targeted by script
  , _those  :: a           -- ^ All other groups
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Partitioned

-- | The groups database handle
acidic :: Lens' (Partitioned a) (AcidState a)

-- | Groups targeted by script
these :: Lens' (Partitioned a) a

-- | All other groups
those :: Lens' (Partitioned a) a


-- | Open groups data from disk
--
-- Searches @'appData'\/groups@ path for groups data. Starts empty
-- if nothing is found
open :: Settings () -> IO (Partitioned Groups)
open settings = do
  let (path, _) = settings & appData <</>~ "groups"
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

  mentioned p gs = let (xs, ys) = M.partitionWithKey p gs in (Groups xs, Groups ys)

-- | Update groups data
--
-- Combines 'these' and 'those' to get full state
commit :: Partitioned Groups -> IO ()
commit db = update (view acidic db) (PutMapping (M.union (view (those.groups) db) (view (these.groups) db)))

-- | Save groups data to disk
close :: Partitioned Groups -> IO ()
close = createCheckpointAndClose . view acidic

-- | Get groups difference
diff :: Eq b => (a -> [b]) -> a -> a -> [b]
diff f = (\\) `on` f

-- | Get all destination filepaths in 'Groups'
files :: Groups -> [FilePath]
files = map filePath . S.elems <=< M.elems . unGR <=< M.elems . view groups

-- | Get all sources location in 'Groups'
sources :: Groups -> [FilePath]
sources = map sourcePath . M.keys . unGR <=< M.elems . view groups


-- | Extract groups data from script
--
-- Won't get /all/ mentioned groups but only those for which there is
-- some useful action to do.
fromScript :: Free (Term Annotate 'Sources) a -> Groups
fromScript script = execState (iterM construct script) (Groups mempty)
 where
  construct :: Term Annotate 'Sources (State Groups a) -> State Groups a
  construct term = case term of
    TS (AS { asProfile, asUser }) (Source sourceType fromLocation sourcePath _) i next -> do
      let record = SR { sourceType, fromLocation, sourcePath, sourceOwner = fmap user asUser }
      groups . at asProfile . non mempty <>= GR (M.singleton record mempty)
      iterM (populate asProfile record) i
      next
    TM _ next -> next

  populate
    :: String                                 -- ^ Profile name
    -> SourceRecord                           -- ^ Source info record
    -> Term Annotate 'Actions (State Groups a) -- ^ Current script term
    -> State Groups a
  populate profile source term = case term of
    TA (AA { aaUser }) action next -> do
      for_ (toRecord action (fmap user aaUser)) $ \record ->
        assign (groups.ix profile.ix source.contains record) True
      next
    TM _ next -> next
   where
    toRecord (Link src dst)       = toFileRecord "link" src dst
    toRecord (Copy src dst _)     = toFileRecord "copy" src dst
    toRecord (Template src dst _) = toFileRecord "template" src dst
    toRecord (Patch src dst _)    = toFileRecord "patch" src dst
    toRecord (Command {})         = const Nothing

    toFileRecord fileType fromSource filePath fileOwner =
      Just FR { fileType, fromSource, filePath, fileOwner }

user :: User -> Either String Int
user (Username s) = Left s
user (UserID n) = Right (fromIntegral n)

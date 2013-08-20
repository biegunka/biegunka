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
  ) where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad.Free (Free(..), iterM)
import           Control.Monad.State (State, execState)
import           Data.Acid
import           Data.Acid.Local
import           Data.Aeson
import           Data.Foldable (elem, for_)
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Prelude hiding (elem)
import           System.FilePath ((</>))

import Control.Biegunka.Settings
  (Settings, Targets(..), appData, targets)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..))

-- $setup
-- >>> import Data.Default


-- | Source data record
data SourceRecord = SR
  { sourceType   :: String   -- 'Source' type: @git@\/@darcs@\/etc.
  , fromLocation :: FilePath -- 'Source' location (url, basically)
  , sourcePath   :: FilePath -- Path to 'Source'
  } deriving (Show, Read)

-- | Only destination filepath matters for equality
instance Eq SourceRecord where
  (==) = (==) `on` sourcePath

-- | Only destination filepath matters for ordering
instance Ord SourceRecord where
  (<=) = (<=) `on` sourcePath

instance ToJSON SourceRecord where
  toJSON SR { sourceType, fromLocation, sourcePath } = object
    [ "type" .= sourceType
    , "from" .= fromLocation
    , "path" .= sourcePath
    ]

deriveSafeCopy 0 'base ''SourceRecord


-- | File data record
data FileRecord = FR
  { fileType   :: String   -- File type: @link@\/@copy@\/etc.
  , fromSource :: FilePath -- File source location (path on disk)
  , filePath   :: FilePath -- Path to file
  } deriving (Show, Read)

-- | Only destination filepath matters for equality
instance Eq FileRecord where
  (==) = (==) `on` filePath

-- | Only destination filepath matters for ordering
instance Ord FileRecord where
  (<=) = (<=) `on` filePath

instance ToJSON FileRecord where
  toJSON FR { fileType, fromSource, filePath } = object
    [ "type" .= fileType
    , "from" .= fromSource
    , "path" .= filePath
    ]

deriveSafeCopy 0 'base ''FileRecord


-- | Group data record
--
-- A mapping from 'Source' data to set of 'FileRecord's
newtype GroupRecord = GR
  { unGR :: Map SourceRecord (Set FileRecord)
  } deriving (Show, Read, Eq, Typeable)

instance Monoid GroupRecord where
  mempty = GR mempty
  GR a `mappend` GR b = GR (a `mappend` b)

type instance Index GroupRecord = SourceRecord
type instance IxValue GroupRecord = Set FileRecord

instance Applicative f => Ixed f GroupRecord where
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
    deriving (Show, Read, Typeable)

instance Monoid Groups where
  mempty = Groups mempty
  Groups xs `mappend` Groups ys = Groups (xs `mappend` ys)

makeLensesWith (defaultRules & generateSignatures .~ False) ''Groups

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

makeLensesWith (defaultRules & generateSignatures .~ False) ''Partitioned

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
  let path = settings^.appData </> "groups"
  acid <- openLocalStateFrom path mempty
  gs   <- query acid GetMapping
  let (xs, ys) = mentioned (settings^.targets) gs
  return (Partitioned { _acidic = acid, _these = xs, _those = ys })
 where
  mentioned All        gs = (Groups gs , Groups mempty)
  mentioned (Subset s) gs =
    let (xs, ys) = M.partitionWithKey (\k _ -> k `elem` s) gs
    in (Groups xs, Groups ys)

-- | Update groups data
--
-- Combines 'these' and 'those' to get full state
commit :: Partitioned Groups -> IO ()
commit db = update (db^.acidic) (PutMapping (M.union (db^.those.groups) (db^.these.groups)))

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
fromScript :: Free (Term Annotate Sources) a -> Groups
fromScript script = execState (iterM construct script) (Groups mempty)
 where
  construct :: Term Annotate Sources (State Groups a) -> State Groups a
  construct term = case term of
    TS (AS { asProfile }) (Source sourceType fromLocation sourcePath _) i next -> do
      let record = SR { sourceType, fromLocation, sourcePath }
      groups . at asProfile . non mempty <>= GR (M.singleton record mempty)
      iterM (populate asProfile record) i
      next
    TM _ next -> next

  populate
    :: String                                 -- ^ Profile name
    -> SourceRecord                           -- ^ Source info record
    -> Term Annotate Actions (State Groups a) -- ^ Current script term
    -> State Groups a
  populate profile source term = case term of
    TA _ action next -> do
      for_ (toRecord action) $ \record ->
        assign (groups.ix profile.ix source.contains record) True
      next
    TM _ next -> next
   where
    toRecord (Link src dst)       = Just FR { fileType = "link",     fromSource = src, filePath = dst }
    toRecord (Copy src dst _)     = Just FR { fileType = "copy",     fromSource = src, filePath = dst }
    toRecord (Template src dst _) = Just FR { fileType = "template", fromSource = src, filePath = dst }
    toRecord (Patch src dst _)    = Just FR { fileType = "patch",    fromSource = src, filePath = dst }
    toRecord (Command {})         = Nothing

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Saved profiles data management
module Control.Biegunka.DB
  ( DB(..), Groups, GroupRecord(..), SourceRecord(..), FileRecord(..)
  , here, there
  , open, merge, save, fromScript
  , diff, files, sources
  ) where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad.Free (Free(..), iterM)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (State, put, execState)
import           Data.Acid
import           Data.Acid.Local
import           Data.Aeson
import           Data.Foldable (for_)
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           System.FilePath ((</>))

import Control.Biegunka.Settings (Settings, appData, targets)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..))

-- $setup
-- >>> import Data.Default


-- | Group record
newtype GroupRecord = GR
  { unGR :: Map SourceRecord (Set FileRecord)
  } deriving (Show, Read, Eq)

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


-- | Source record
data SourceRecord = SR
  { sourceType   :: String
  , fromLocation :: FilePath
  , sourcePath   :: FilePath
  } deriving (Show, Read)

-- | Only destination filepath matters
instance Eq SourceRecord where
  (==) = (==) `on` sourcePath

-- | Only destination filepath matters
instance Ord SourceRecord where
  (<=) = (<=) `on` sourcePath

instance ToJSON SourceRecord where
  toJSON SR { sourceType, fromLocation, sourcePath } = object
    [ "type" .= sourceType
    , "from" .= fromLocation
    , "path" .= sourcePath
    ]


-- | File record
data FileRecord = FR
  { fileType   :: String
  , fromSource :: FilePath
  , filePath   :: FilePath
  } deriving (Show, Read)

-- | Only destination filepath matters
instance Eq FileRecord where
  (==) = (==) `on` filePath

-- | Only destination filepath matters
instance Ord FileRecord where
  (<=) = (<=) `on` filePath

instance ToJSON FileRecord where
  toJSON FR { fileType, fromSource, filePath } = object
    [ "type" .= fileType
    , "from" .= fromSource
    , "path" .= filePath
    ]

deriveSafeCopy 0 'base ''FileRecord
deriveSafeCopy 0 'base ''SourceRecord
deriveSafeCopy 0 'base ''GroupRecord


-- | All groups data
newtype Groups = Groups { _groups :: Map String GroupRecord }
    deriving (Show, Read, Typeable)

deriveSafeCopy 0 'base ''Groups

defGroups :: Groups
defGroups = Groups mempty

getGroups :: Query Groups Groups
getGroups = ask

putGroups :: Groups -> Update Groups ()
putGroups = put

makeAcidic ''Groups ['getGroups, 'putGroups]

makeLenses ''Groups


-- | Biegunka 'DB'
data DB = DB
  { _acidic :: AcidState Groups -- ^ The whole database
  , _here   :: Groups           -- ^ Part of database targeted by current script
  , _there  :: Groups           -- ^ The other part of database
  }

makeLenses ''DB

-- | Open groups' data
open :: Settings () -> IO DB
open settings = do
  let path = settings^.appData </> "groups"
  acid      <- openLocalStateFrom path defGroups
  Groups gs <- query acid GetGroups
  let (thises, thats) = M.partitionWithKey (\k _ -> elemOf (targets.folded) k settings) gs
  return (DB acid (Groups thises) (Groups thats))

-- | Update groups' data
merge :: DB -> Groups -> IO ()
merge db (Groups gs) =
  update (db^.acidic) (PutGroups (Groups $ M.union (db^.there.groups) gs))

-- | Save groups' data
save :: DB -> IO ()
save = createCheckpointAndClose . view acidic

-- | Get groups difference
diff :: Eq b => (a -> [b]) -> a -> a -> [b]
diff f = (\\) `on` f

-- | All destination files paths
files :: Groups -> [FilePath]
files = map filePath . S.elems <=< M.elems . unGR <=< M.elems . view groups

-- | All sources paths
sources :: Groups -> [FilePath]
sources = map sourcePath . M.keys . unGR <=< M.elems . view groups


-- | Extract groups' data from script
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

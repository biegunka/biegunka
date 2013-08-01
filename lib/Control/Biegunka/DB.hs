{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Saved profiles data management
module Control.Biegunka.DB
  ( DB(..), GroupRecord(..), SourceRecord(..), FileRecord(..)
  , load, loads, save, fromScript
  , filepaths, sources
  ) where

import Control.Applicative
import Control.Monad ((<=<), forM, mplus)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad.Free (Free(..), iterM)
import           Control.Monad.State (State, execState)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy (fromStrict)
import           Data.Foldable (Foldable, for_, toList)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import           System.Directory (createDirectoryIfMissing, removeDirectory, removeFile)
import           System.FilePath ((</>), (<.>))
import           System.FilePath.Lens (directory)

import Control.Biegunka.Settings (Settings, appData)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..))

-- $setup
-- >>> import Data.Default


-- | All groups data
newtype DB = DB
  { _db :: Map String GroupRecord
  } deriving (Show, Read)


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

instance FromJSON GroupRecord where
  parseJSON obj = GR . M.fromList <$> do
    o       <- parseJSON obj
    sources <- o .: "sources"
    forM sources $ \s -> do
      t  <- s .: "info"
      fs <- (s .: "files" >>= mapM parseF) <|> (s .: "files" >>= mapM (fmap snd . parseFF))
      return (t, S.fromList fs)
   where
    parseFF :: Value -> Parser (FilePath, FileRecord)
    parseFF = parseJSON

    parseF :: Value -> Parser FileRecord
    parseF = parseJSON

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

instance FromJSON SourceRecord where
  parseJSON (Object o) = SR
    <$> (o .: "type" <|> o .: "recordtype")
    <*> (o .: "from" <|> o .: "base")
    <*> (o .: "path" <|> o .: "location")
  parseJSON _ = empty

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

instance FromJSON FileRecord where
  parseJSON (Object o) = FR
    <$> (o .: "type" <|> o .: "recordtype")
    <*> (o .: "from" <|> o .: "base")
    <*> (o .: "path" <|> o .: "location")
  parseJSON _ = empty

instance ToJSON FileRecord where
  toJSON FR { fileType, fromSource, filePath } = object
    [ "type" .= fileType
    , "from" .= fromSource
    , "path" .= filePath
    ]


makeLensesWith (defaultRules & generateSignatures .~ False) ''DB

-- | Profiles data
db :: Lens' DB (Map String GroupRecord)


-- | Load profiles mentioned in script
load :: Foldable t => Settings () -> t String -> IO DB
load c = fmap (DB . M.fromList) . loads c . toList

-- | Load profile data from disk
loads :: Settings () -> [String] -> IO [(String, GroupRecord)]
loads c (p:ps) = do
  let name = profileFilePath c p
  Just v <- (parseMaybe parseJSON <=< decode . fromStrict) <$> B.readFile name
  ((p,v):) <$> loads c ps
 `mplus`
  loads c ps
loads _ [] = return []


-- | Save profiles data to files.
--
-- Each profile is mapped to a separate file in the 'appData' directory.
save :: Settings () -> Set String -> DB -> IO ()
save c ps (DB b) = do
  -- Create app data directory if it's missing
  createDirectoryIfMissing False (c^.appData)
  -- Save profiles new data
  ifor_ b $ \p sourceData -> do
    let name = profileFilePath c p
    -- Create missing directories for nested profile files
    createDirectoryIfMissing True (name^.directory)
    -- Finally encode profile as JSON
    BL.writeFile name $ encode sourceData
  -- Remove mentioned but empty profiles data
  for_ (ps \\ M.keysSet b) $ \p -> do
    let name = profileFilePath c p
        dirs = name^..directory.takingWhile (/= c^.appData) (iterated (^.directory))
    removeFile name
    -- Also remove empty directories if possible
    mapM_ removeDirectory dirs
   `mplus`
    -- Ignore failures, they are not critical in any way here
    return ()


-- | Compute profiles' filepaths with current settings
--
-- >>> let settings = def :: Settings ()
--
-- >>> profileFilePath settings ""
-- "~/.biegunka/profiles/.profile"
--
-- >>> profileFilePath settings "dotfiles"
-- "~/.biegunka/profiles/dotfiles.profile"
profileFilePath :: Settings a -> String -> FilePath
profileFilePath settings name =
  settings^.appData.to (\app -> app </> "profiles" </> name <.> "profile")


-- | All destination files paths
filepaths :: DB -> [FilePath]
filepaths = map filePath . S.elems <=< M.elems . unGR <=< M.elems . view db

-- | All sources paths
sources :: DB -> [FilePath]
sources = map sourcePath . M.keys . unGR <=< M.elems . view db


-- | Extract terms data from script
fromScript :: Free (Term Annotate Sources) a -> DB
fromScript script = execState (iterM construct script) (DB mempty)
 where
  construct :: Term Annotate Sources (State DB a) -> State DB a
  construct term = case term of
    TS (AS { asProfile }) (Source sourceType fromLocation sourcePath _) i next -> do
      let record = SR { sourceType, fromLocation, sourcePath }
      db . at asProfile . non mempty <>= GR (M.singleton record mempty)
      iterM (populate asProfile record) i
      next
    TM _ next -> next

  populate
    :: String                             -- ^ Profile name
    -> SourceRecord                       -- ^ Source info record
    -> Term Annotate Actions (State DB a) -- ^ Current script term
    -> State DB a
  populate profile source term = case term of
    TA _ action next -> do
      for_ (toRecord action) $ \record ->
        assign (db.ix profile.ix source.contains record) True
      next
    TM _ next -> next
   where
    toRecord (Link src dst)       = Just FR { fileType = "link",     fromSource = src, filePath = dst }
    toRecord (Copy src dst _)     = Just FR { fileType = "copy",     fromSource = src, filePath = dst }
    toRecord (Template src dst _) = Just FR { fileType = "template", fromSource = src, filePath = dst }
    toRecord (Patch src dst _)    = Just FR { fileType = "patch",    fromSource = src, filePath = dst }
    toRecord (Command {})         = Nothing

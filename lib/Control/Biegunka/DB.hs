{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- | Saved profiles data management
module Control.Biegunka.DB
  ( DB(..), SourceRecord(..), FileRecord(..)
  , load, loads, save, fromScript
  , filepaths, sources
  ) where

import Control.Applicative
import Control.Monad ((<=<), forM, mplus)
import Data.Function (on)
import Data.Monoid (Monoid(..))

import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, execState)
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy (fromStrict)
import           Data.Foldable (for_, toList)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import           System.Directory (createDirectoryIfMissing, removeDirectory, removeFile)
import           System.FilePath ((</>), (<.>))
import           System.FilePath.Lens (directory)

import Control.Biegunka.Control (Settings, appData)
import Control.Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Control.Biegunka.Script (Annotate(..))

-- $setup
-- >>> import Data.Default


-- | Profiles data
newtype DB = DB
  { _db :: Map String (Map SourceRecord (Set FileRecord))
  } deriving (Show, Read, Monoid)


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
    <$> (o .: "recordtype" <|> o .: "type")
    <*> (o .: "base"       <|> o .: "from")
    <*> (o .: "location"   <|> o .: "path")
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
    <$> (o .: "recordtype" <|> o .: "type")
    <*> (o .: "base"       <|> o .: "from")
    <*> (o .: "location"   <|> o .: "path")
  parseJSON _ = empty

instance ToJSON FileRecord where
  toJSON FR { fileType, fromSource, filePath } = object
    [ "type" .= fileType
    , "from" .= fromSource
    , "path" .= filePath
    ]

makeLensesWith (defaultRules & generateSignatures .~ False) ''DB

-- | Profiles data
db :: Lens' DB (Map String (Map SourceRecord (Set FileRecord)))


-- | Load profiles mentioned in script
load :: Settings () -> Set String -> IO DB
load c = fmap (DB . M.fromList) . loads c . toList

-- | Load profile data from disk
loads :: Settings () -> [String] -> IO [(String, Map SourceRecord (Set FileRecord))]
loads c (p:ps) = do
  let name = profileFilePath c p
  Just v <- (parseMaybe parser <=< decode . fromStrict) <$> B.readFile name
  (v:) <$> loads c ps
 `mplus`
  loads c ps
 where
  parser (Object o) = (p, ) . M.fromList <$> do
    ss <- o .: "sources"
    forM ss $ \s -> do
      t  <- s .: "info"
      fs <- (s .: "files" >>= mapM (fmap snd . parseFF)) <|> (s .: "files" >>= mapM parseF)
      return (t, S.fromList fs)
  parser _ = empty

  parseFF :: Value -> Parser (FilePath, FileRecord)
  parseFF = parseJSON

  parseF :: Value -> Parser FileRecord
  parseF = parseJSON
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
    BL.writeFile name $ encode' sourceData
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
 where
  encode' = T.encodeUtf8 . T.toLazyText . fromValue . unparser
  unparser t  = object [             "sources" .= map repo   (M.toList t)]
  repo (k, v) = object ["info" .= k, "files"   .= map toJSON (S.toList v)]


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
filepaths = map filePath . S.elems <=< M.elems <=< M.elems . view db

-- | All sources paths
sources :: DB -> [FilePath]
sources = map sourcePath . M.keys <=< M.elems . view db


-- | Extract terms data from script
fromScript :: Free (Term Annotate Sources) a -> DB
fromScript z = execState (f z) mempty
 where
  f :: Free (Term Annotate Sources) a -> State DB ()
  f (Free (TS (AS { asProfile = p }) (Source t u d _) i x)) = do
    let s = SR { sourceType = t, fromLocation = u, sourcePath = d }
    db . at p . non mempty <>= M.singleton s mempty
    g p s i
    f x
  f (Free (TM _ x)) = f x
  f (Pure _) = return ()

  g :: String -> SourceRecord -> Free (Term Annotate Actions) a -> State DB ()
  g p s (Free (TA _ a x)) = do
    db . at p . traverse . at s . traverse <>= h a
    g p s x
   where
    h (Link src dst)       = S.singleton FR { fileType = "link", fromSource = src, filePath = dst }
    h (Copy src dst _)     = S.singleton FR { fileType = "copy", fromSource = src, filePath = dst }
    h (Template src dst _) = S.singleton FR { fileType = "template", fromSource = src, filePath = dst }
    h (Patch src dst _)    = S.singleton FR { fileType = "patch", fromSource = src, filePath = dst }
    h (Command {})         = mempty
  g p s (Free (TM _ x)) = g p s x
  g _ _ (Pure _) = return ()

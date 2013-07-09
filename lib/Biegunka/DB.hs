{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- | Saved profiles data management
module Biegunka.DB
  ( Biegunka(..), R(..)
  , load, loads, save, construct
  , filepaths, sources
  ) where

import Control.Applicative
import Control.Monad ((<=<), forM, mplus)
import Data.Monoid (Monoid(..))

import           Control.Lens hiding ((.=), (<.>))
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (State, execState)
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import           Data.ByteString.Lazy (fromStrict)
#endif
import           Data.Default
import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import           System.Directory (createDirectoryIfMissing, removeDirectory, removeFile)
import           System.FilePath ((</>), (<.>))
import           System.FilePath.Lens (directory)

import Biegunka.Control (Settings, appData)
import Biegunka.Language (Scope(..), Term(..), Source(..), Action(..))
import Biegunka.Script (Annotate(..))


-- | Profiles data
newtype Biegunka = Biegunka
  { unBiegunka :: Map String (Map R (Map FilePath R))
  } deriving (Show, Read, Eq, Ord, Monoid)


-- | Source record
data R = R
  { recordtype :: String
  , base :: FilePath
  , location :: FilePath
  } deriving (Show, Read, Eq, Ord)

instance FromJSON R where
  parseJSON (Object o) = liftA3 R (o .: "recordtype") (o .: "base") (o .: "location")
  parseJSON _          = empty

instance ToJSON R where
  toJSON R { recordtype = ft, base = bs, location = lc } = object
    [ "recordtype" .= ft
    , "base" .= bs
    , "location" .= lc
    ]


-- | Profiles data construction state
data Construct = Construct
  { _profile :: String                               -- ^ Current profile
  , _source :: R                                     -- ^ Current source
  , _biegunka :: Map String (Map R (Map FilePath R)) -- ^ Already constructed mapping
  } deriving (Show, Read, Eq, Ord)

instance Default Construct where
  def = Construct
    { _profile  = mempty
    , _source   = (R "" "" "")
    , _biegunka = mempty
    }

makeLensesWith (defaultRules & generateSignatures .~ False) ''Construct

-- | Current profile
profile :: Lens' Construct String

-- | Current source
source :: Lens' Construct R

-- | Already constructed mapping
biegunka :: Lens' Construct (Map String (Map R (Map FilePath R)))


-- | Load profiles mentioned in script
load :: Settings () -> Set String -> IO Biegunka
load c = fmap (Biegunka . M.fromList) . loads c . toList


-- | Load profile data from file
--
-- This may fail, on failure 'loadProfile' returns Nothing
--
-- Reasons to fail:
--
--  * Cannot read from profile file (various reasons here)
--
--  * Cannot parse profile file (wrong format)
loads :: Settings () -> [String] -> IO [(String, Map R (Map FilePath R))]
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
      fs <- s .: "files" >>= mapM parseJSON
      return (t, M.fromList fs)
  parser _ = empty
loads _ [] = return []


-- | Save profiles data to files.
--
-- Each profile is mapped to a separate file in 'appData' directory.
-- Mapping rules are simple: profile name is a relative path in 'appData'.
--
-- For example, profile @dotfiles@ is located in @~\/.biegunka\/dotfiles@ by default
-- and profile @my\/dotfiles@ is located in @~\/.biegunka.my\/dotfiles@ by default.
save :: Settings () -> Biegunka -> IO ()
save c (Biegunka b) = do
  createDirectoryIfMissing False (view appData c)
  ifor_ b $ \p sourceData -> do
    let name = profileFilePath c p
        dir = view directory name
        dirs = dir ^.. takingWhile (/= view appData c) (iterated (view directory))
    if M.null sourceData then do
      removeFile name            -- Since profile is empty no need having crap in the filesystem
      mapM_ removeDirectory dirs -- Also remove empty directories if possible
     `mplus`
      return ()                  -- Ignore failures, they are not critical in any way here
    else do
      createDirectoryIfMissing True dir      -- Create missing directories for nested profile files
      BL.writeFile name $ encode' sourceData -- Finally encode profile as JSON
 where
  encode' = T.encodeUtf8 . T.toLazyText . fromValue . unparser
  unparser t  = object [             "sources" .= map repo   (M.toList t)]
  repo (k, v) = object ["info" .= k, "files"   .= map toJSON (M.toList v)]


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
filepaths :: Biegunka -> [FilePath]
filepaths = M.keys <=< M.elems <=< M.elems . unBiegunka

-- | All sources paths
sources :: Biegunka -> [FilePath]
sources = map location . M.keys <=< M.elems . unBiegunka


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
-- | Convert strict bytestring into lazy one
fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return
#endif


-- | Extract terms data from script
construct :: Free (Term Annotate Sources) a -> Biegunka
construct = Biegunka . _biegunka . (`execState` def) . go
 where
  go :: Free (Term Annotate s) a -> State Construct ()
  go (Free (TS (AS { asProfile = p }) (Source t u d _) i z)) = do
    let s = R { recordtype = t, base = u, location = d }
    biegunka . at p . non mempty <>= M.singleton s mempty
    assign profile p
    assign source s
    go i
    go z
  go (Free (TA _ a z)) = do
    p <- use profile
    s <- use source
    biegunka . at p . traverse . at s . traverse <>= h a
    go z
   where
    h (Link src dst)       = M.singleton dst R { recordtype = "link",       base = src, location = dst }
    h (Copy src dst)       = M.singleton dst R { recordtype = "copy",       base = src, location = dst }
    h (Template src dst _) = M.singleton dst R { recordtype = "template",   base = src, location = dst }
    h (Command {})           = mempty
  go _ = return ()

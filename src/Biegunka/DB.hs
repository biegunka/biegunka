{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import           System.Directory (createDirectoryIfMissing, removeDirectory, removeFile)
import           System.FilePath.Lens

import Biegunka.Control (Controls, appData)
import Biegunka.Language (Scope(..), EL(..), P(..), S(..), A(..))
import Biegunka.Script (SA(..))


newtype Biegunka = Biegunka
  { unBiegunka :: Map String (Map R (Map FilePath R))
  } deriving (Show, Read, Eq, Ord, Monoid)


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


data Construct = Construct
  { _source :: R
  , _profile :: String
  , _biegunka :: Map String (Map R (Map FilePath R))
  } deriving (Show, Read, Eq, Ord)

instance Default Construct where
  def = Construct (R "" "" "") mempty mempty

profileL :: Lens' Construct String
profileL f (Construct s p b) = (\p' -> Construct s p' b) <$> f p

sourceL :: Lens' Construct R
sourceL f (Construct s p b) = (\s' -> Construct s' p b) <$> f s

biegunkaL :: Lens' Construct (Map String (Map R (Map FilePath R)))
biegunkaL f (Construct s p b) = (\b' -> Construct s p b') <$> f b


load :: Controls -> Free (EL SA Profiles) a -> IO Biegunka
load c = fmap (Biegunka . M.fromList) . loads c . profiles
 where
  profiles :: Free (EL SA Profiles) a -> [String]
  profiles (Free (EP _ (P n) _ x)) = n : profiles x
  profiles (Free (EM _ x)) = profiles x
  profiles (Pure _) = []


-- | Load profile data from file
--
-- This may fail, on failure 'loadProfile' returns Nothing
--
-- Reasons to fail:
--
--  * Cannot read from profile file (various reasons here)
--
--  * Cannot parse profile file (wrong format)
loads :: Controls -> [String] -> IO [(String, Map R (Map FilePath R))]
loads c (p:ps) = do
  let (name, _) = c & appData <</>~ p
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
save :: Controls -> Biegunka -> IO ()
save c (Biegunka b) = do
  createDirectoryIfMissing False (view appData c)
  ifor_ b $ \profile sourceData -> do
    let (name, _) = c & appData <</>~ profile -- Map profile to file name
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


filepaths :: Biegunka -> [FilePath]
filepaths = M.keys <=< M.elems <=< M.elems . unBiegunka


sources :: Biegunka -> [FilePath]
sources = map location . M.keys <=< M.elems . unBiegunka


fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return


construct :: Free (EL SA s) a -> Biegunka
construct = Biegunka . _biegunka . (`execState` def) . go
 where
  go :: Free (EL SA s) a -> State Construct ()
  go (Free (EP _ (P n) i z)) = do
    biegunkaL . at n . anon mempty (const False) <>= mempty
    assign profileL n
    go i
    go z
  go (Free (ES _ (S t u d _) i z)) = do
    let s = R { recordtype = t, base = u, location = d }
    n <- use profileL
    assign sourceL s
    biegunkaL . at n . non mempty <>= M.singleton s mempty
    go i
    go z
  go (Free (EA _ a z)) = do
    n <- use profileL
    s <- use sourceL
    biegunkaL . at n . traverse . at s . traverse <>= h a
    go z
   where
    h (Link src dst)       = M.singleton dst R { recordtype = "link",       base = src, location = dst }
    h (Copy src dst)       = M.singleton dst R { recordtype = "copy",       base = src, location = dst }
    h (Template src dst _) = M.singleton dst R { recordtype = "template",   base = src, location = dst }
    h (Shell {})           = mempty
  go _ = return ()

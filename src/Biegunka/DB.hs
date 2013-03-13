{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.DB
  ( Biegunka(..), R(..)
  , load, loadProfile, save, construct
  , filepaths, sources
  ) where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (Monoid(..))
import System.IO.Error (catchIOError)

import           Control.Lens hiding ((.=), (<.>))
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
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath.Lens

import Biegunka.Control (Controls, appData)
import Biegunka.Language.External (A(..))
import Biegunka.Language.Internal


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
  toJSON R { recordtype = ft, base = bs, location = lc } = object [ "recordtype" .= ft, "base" .= bs, "location" .= lc ]


data Construct = Construct
  { _source :: R
  , _biegunka :: Map String (Map R (Map FilePath R))
  } deriving (Show, Read, Eq, Ord)

instance Default Construct where
  def = Construct (R "" "" "") mempty

makeLenses ''Construct


load :: Controls -> [IL] -> IO Biegunka
load c = fmap (Biegunka . M.fromList . catMaybes) . mapM (loadProfile c) . mapMaybe profiles
 where
  profiles (IS _ _ _ _ n _) = Just n
  profiles _                = Nothing


loadProfile :: Controls -> String -> IO (Maybe (String, Map R (Map FilePath R)))
loadProfile c n = do
  let (z, _) = c & appData <</>~ n
  flip catchIOError (\_ -> return Nothing) $
    (parseMaybe parser <=< decode . fromStrict) <$> B.readFile z
 where
  parser (Object o) = (,) n .  M.fromList <$> (mapM repo =<< o .: "sources")
   where
    repo z = do
      t <- z .: "info"
      fs <- z .: "files" >>= mapM parseJSON
      return (t, M.fromList fs)
  parser _ = empty


save :: Controls -> Biegunka -> IO ()
save c (Biegunka b) = do
  createDirectoryIfMissing False (c ^. appData)
  ifor_ b $ \k v ->
    let (n, _) = c & appData <</>~ k in
    if M.null v
      then removeFile n `catchIOError` \_ -> return ()
      else BL.writeFile n . T.encodeUtf8 . T.toLazyText . fromValue $ unparser v
 where
  unparser t = object ["sources" .= map repo (M.toList t)]
   where
    repo (k, v) = object ["info" .= k, "files" .= map toJSON (M.toList v)]


filepaths :: Biegunka -> [FilePath]
filepaths = M.keys <=< M.elems <=< M.elems . unBiegunka


sources :: Biegunka -> [FilePath]
sources = map location . M.keys <=< M.elems . unBiegunka


fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return


construct :: [IL] -> Biegunka
construct = Biegunka . _biegunka . (`execState` def) . mapM_ g
 where
  g :: IL -> State Construct ()
  g (IS dst t _ _ pn sn) = do
    let s = R { recordtype = t, base = sn, location = dst }
    assign source s
    biegunka . at pn . non mempty <>= M.singleton s mempty
  g (IA a _ pn _) = do
    s <- use source
    biegunka . at pn . traverse . at s . traverse <>= h a
   where
    h (Link src dst)       = M.singleton dst R { recordtype = "link",       base = src, location = dst }
    h (Copy src dst)       = M.singleton dst R { recordtype = "copy",       base = src, location = dst }
    h (Template src dst _) = M.singleton dst R { recordtype = "template",   base = src, location = dst }
    h (Shell {})           = mempty
  g (IW _) = return ()

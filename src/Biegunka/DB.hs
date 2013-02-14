{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.DB
  ( Biegunka
  , load, save, construct
  , filepaths, sources
  ) where

import Control.Applicative ((<$>), empty)
import Control.Monad ((<=<))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (Monoid(..))
import System.IO.Error (catchIOError)

import           Control.Lens hiding ((.=), (<.>))
import qualified Control.Lens as CL
import           Control.Monad.State (State, execState)
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import           System.Directory (removeFile)
import           System.FilePath ((</>), (<.>))

import Biegunka.Language


newtype Biegunka = Biegunka
  { unBiegunka :: Map String (Map FilePath (Set FilePath))
  } deriving (Show, Eq, Monoid)


data Construct = Construct
  { _profile :: String
  , _source :: FilePath
  , _biegunka :: Map String (Map FilePath (Set FilePath))
  }

instance Default Construct where
  def = Construct mempty mempty mempty

makeLenses ''Construct


load :: FilePath -> [Command l a b] -> IO Biegunka
load r = fmap (Biegunka . M.fromList . catMaybes) . mapM readProfile . mapMaybe profiles
 where
  profiles (P name _ _) = Just name
  profiles _ = Nothing

  readProfile k = flip catchIOError (\_ -> return Nothing) $
    (parseMaybe (parser k) <=< decode . fromStrict) <$> B.readFile (r </> ".biegunka" <.> k)

  parser k (Object o) = (,) k .  M.fromList <$> (mapM repo =<< o .: "sources")
   where
    repo z = do
      n <- z .: "path"
      fs <- z .: "files"
      return (n, S.fromList fs)
  parser _ _ = empty


save :: FilePath -> Biegunka -> IO ()
save r (Biegunka b) = traverseWithKey_ b $ \k v ->
  let n = r </> ".biegunka" <.> k in
  if M.null v
    then removeFile n `catchIOError` \_ -> return ()
    else BL.writeFile n . T.encodeUtf8 . T.toLazyText . fromValue $ unparser v
 where
  traverseWithKey_ m f = itraverse f m >> return ()

  unparser t = object ["sources" .= map repo (M.toList t)]
   where
    repo (k, v) = object ["path" .= k, "files" .= S.toList v]


filepaths :: Biegunka -> [FilePath]
filepaths = S.toList <=< M.elems <=< M.elems . unBiegunka


sources :: Biegunka -> [FilePath]
sources = M.keys <=< M.elems . unBiegunka


fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return


construct :: [Command l () b] -> Biegunka
construct = Biegunka . _biegunka . (`execState` def) . mapM_ g
 where
  g :: Command l () b -> State Construct ()
  g (P name _ _) = do
    profile CL..= name
    biegunka . at name ?= mempty
  g (S _ s _ _ _) = do
    p <- use profile
    source CL..= s
    biegunka . at p . traverse . at s ?= mempty
  g (F a _) = do
    p <- use profile
    s <- use source
    biegunka . at p . traverse . at s . traverse <>= h a
   where
    h (RegisterAt _ dst) = S.singleton dst
    h (Link _ dst) = S.singleton dst
    h (Copy _ dst) = S.singleton dst
    h (Template _ dst _) = S.singleton dst
    h (Shell {}) = mempty
  g (W _ _) = return ()
